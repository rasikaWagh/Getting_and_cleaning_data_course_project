## This script creates a tidy data set of UCI HAR Data Set

library(dplyr)
library(stringr)

setwd("C:...\getting and cleaning data\\Assignement 4\\UCI HAR Dataset")

# Read training data into a train file 
trainFileName <- "train\\X_train.txt"
trainFile<- read.table(trainFileName)

# read test data into a test file 
testFileName <- "test\\X_test.txt"
testFile<- read.table(testFileName)

# read training data labels 
trainLabelFileName <- "train\\y_train.txt"
trainLabelFile <- read.table(trainLabelFileName, colClasses = "character")

# read test data labels 
testLabelFileName <- "test\\y_test.txt"
testLabelFile <- read.table(testLabelFileName, colClasses = "character")

# read the train subject file 
trainSubjectFileName <- "train\\subject_train.txt"
trainSubjectFile <- read.table(trainSubjectFileName)


# read the test subject file 
testSubjectFileName <- "test\\subject_test.txt"
testSubjectFile <- read.table(testSubjectFileName)
subjectFile <- rbind(trainSubjectFile,testSubjectFile)

# read the feature info file 
featureFileName <- "features.txt"
featureFile <- read.table(featureFileName,colClasses = c("numeric","character"),col.names = c("srNo","featureName"))

# read the activity label file 
activityLabelFileName <- "activity_labels.txt"
activityLabelFile <- read.table(activityLabelFileName,colClasses = c("numeric","character"),col.names = c("srNo", "activityName"))

trainLabelFile$V1 <- str_replace_all(trainLabelFile$V1,c("1"=activityLabelFile$activityName[1],
                                  "2"=activityLabelFile$activityName[2],
                                  "3"=activityLabelFile$activityName[3],
                                  "4"=activityLabelFile$activityName[4],
                                  "5"=activityLabelFile$activityName[5],
                                  "6"=activityLabelFile$activityName[6]));

testLabelFile$V1 <- str_replace_all(testLabelFile$V1,c("1"=activityLabelFile$activityName[1],
                                                         "2"=activityLabelFile$activityName[2],
                                                         "3"=activityLabelFile$activityName[3],
                                                         "4"=activityLabelFile$activityName[4],
                                                         "5"=activityLabelFile$activityName[5],
                                                         "6"=activityLabelFile$activityName[6]));

activityLabels <- rbind(trainLabelFile,testLabelFile)
# Select the mean features
meanFeatureIndices <- grep("mean", featureFile$featureName)

# select the std features
stdFeatureIndices <- grep("std", featureFile$featureName)
selectedFeatureIndices <- c(meanFeatureIndices, stdFeatureIndices)

# create cleaned vectors of these feature indices 
meanFeatureNames <- featureFile[meanFeatureIndices,]
stdFeatureNames <- featureFile[stdFeatureIndices,]


# Clean the feature Names 
meanFeatureNames$featureName <- gsub("-|\\(|\\)","",meanFeatureNames$featureName)
stdFeatureNames$featureName <- gsub("-|\\(|\\)","",stdFeatureNames$featureName)
cleanedFeatureNames <- rbind(meanFeatureNames, stdFeatureNames)

# select the sub set of columns form the test and train data set 
targetTrainData <- trainFile[,selectedFeatureIndices]
targetTestData <- testFile[,selectedFeatureIndices]

targetDataSet <- rbind(targetTrainData,targetTestData)

# Append the subject Id and activity Labels in the target data set
targetDataSet <- cbind(subjectFile, activityLabels, targetDataSet)

# Assign the column names of the data frame 
colnames(targetDataSet)<- c("subjectID","activityLabel",cleanedFeatureNames$featureName)
write.table(targetDataSet, file = "HARTidyDataSet.")

averageDataset <- aggregate(. ~subjectID + activityLabel, targetDataSet, mean)
averageDataset <- averageDataset[order(averageDataset$subjectID, averageDataset$activityLabel),]

write.table(averageDataset, file = "averageDatasetBySubjectAndActivity.txt")


