run_analysis <- function(){
    library(data.table)
    library(dplyr)
    
    ## reads files and creates data tables with appropriate labels
    setwd("/Users/Nathan/Desktop/R/CleaningData/UCI HAR Dataset")
    
    features <- data.table::fread("./features.txt", col.names = c("n", "feature"))
    activities <- data.table::fread("./activity_labels.txt", col.names = c("class label", "activity"))
    
    x_test <- data.table::fread("./test/X_test.txt", col.names = as.character(features$feature)) ##each column corresponds to a features row
    y_test <- data.table::fread("./test/y_test.txt", col.names = ("class label")) ##labels to activities
    subject_test <- data.table::fread("./test/subject_test.txt" , col.names = ("subject"))
    
    x_train <- data.table::fread("./train/X_train.txt", col.names = as.character(features$feature)) ##each column corresponds to a features row
    y_train <- data.table::fread("./train/y_train.txt", col.names = ("class label")) ##labels to activites
    subject_train <- data.table::fread("./train/subject_train.txt", col.names = ("subject"))
    
    ## joins data tables
    testDT <- cbind(x_test, y_test, subject_test)
    trainDT <- cbind(x_train, y_train, subject_train)
    mergedDT <- rbind(testDT, trainDT)
    
    ## extracts mean and standard deviation values from data table
    meanCol <- subset(mergedDT, select = grep("mean", names(mergedDT)))
    stdCol <- subset(mergedDT, select = grep("std", names(mergedDT)))
    meanStdDT <- cbind(meanCol, stdCol, mergedDT$`class label`, mergedDT$subject)
    
    ## names activites and labels variables
    activityDT <- merge(meanStdDT, activities, by.x = "V3", by.y = "class label")
    activityDT <- setnames(activityDT, "V4", "subject")
    activityDT <- select(activityDT, -"V3")
    activityDT <- mutate(activityDT, subject = as.factor(subject))
    
    ## calculates average of each variable for each activity and each subject
    finalDT <- activityDT %>% group_by(subject, activity) %>% summarise_all(funs(mean))
    write.table(finalDT, file = "FinalDT.txt", row.names = FALSE)
}
