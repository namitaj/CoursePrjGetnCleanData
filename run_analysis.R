##Prerequisites : 
## 1. The dataset files are unzipped in the directory UCI HAR Dataset 
##    present in working directory 
## 2. The package dplyr is installed

## Clearing the global environment.
 rm(list=ls())
 
## Loading the dplyr package
 library(dplyr)

## Loading the sample data to view the data information and details
## 
## X_train_sample <- read.table("UCI HAR Dataset/train/X_train.txt",nrows=10)
## head(X_train_sample)
## tail(X_train_sample)
## Y_train_sample <- read.table("UCI HAR Dataset/train/Y_train.txt",nrows=10)
## head(Y_train_sample)
## tail(Y_train_sample)
## Subject_train_sample <- read.table("UCI HAR Dataset/train/subject_train.txt",nrows=10)
## head(Subject_train_sample)

## STEP 1: Merging the training and the test sets to create one data set.
 ##Creating data frame for each of the datasets with relevant headers  

Xtrain <- read.table("UCI HAR Dataset/train/X_train.txt",col.names = paste0("fvsd",1:561))
## head(Xtrain)
Ytrain <- read.table("UCI HAR Dataset/train/Y_train.txt",col.names = ("ActivityID"))
## head(Ytrain)
Subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt",col.names = ("SubjectID"))
## head(Subjecttrain)
Features <- read.table("UCI HAR Dataset/features.txt",col.names = (c("FeatureID","Feature")))
## head(Features)
Activity <- read.table("UCI HAR Dataset/activity_labels.txt",col.names = (c("ActivityID","Activity")))
## head(Activity)
Xtest <- read.table("UCI HAR Dataset/test/X_test.txt",col.names = paste0("fvsd",1:561))
## head(Xtest)
Ytest <- read.table("UCI HAR Dataset/test/Y_test.txt",col.names = ("ActivityID"))
## head(Ytrain)
Subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt",col.names = ("SubjectID"))
## head(Subjecttest)
## Combining ActivityID,SubjectID and feature observations of train data to create a new dataset traindata
traindata <- cbind(Subjecttrain,Ytrain,Xtrain)
## Combining ActivityID,SubjectID and feature observations of test data to create a new dataset testdata
testdata <- cbind(Subjecttest,Ytest,Xtest)
## Combining the train and test data sets together using rbind
data <- rbind(traindata,testdata)

##Step 3: Activity description is included in the data 
data2 <- merge(data,Activity,by = "ActivityID")

##Step 2: Extract only the measurements of mean and standard deviation. The features included end with mean() or std()
## Filter the features for mean and standard deviation 
Features_m_s <- paste0("fvsd",Features$FeatureID[grepl("mean()",Features$Feature,fixed=TRUE) |grepl("std()",Features$Feature,fixed=TRUE)])
colNames <-  c("SubjectID","Activity",Features_m_s)
## Selecting only the mean and std data 
data2 <- data2[,names(data2) %in% colNames]
data2 <- data2[,c(1,68,2:67)]

##Step4 : Renaming the columns in the dataset
 names(data2) <- c("Subject","Activity","BodyAccmeanTimeX","BodyAccmeanTimeY","BodyAccmeanTimeZ","BodyAccstdTimeX","BodyAccstdTimeY","BodyAccstdTimeZ","GravityAccmeanTimeX","GravityAccmeanTimeY","GravityAccmeanTimeZ","GravityAccstdTimeX","GravityAccstdTimeY","GravityAccstdTimeZ","BodyAccJerkmeanTimeX","BodyAccJerkmeanTimeY","BodyAccJerkmeanTimeZ","BodyAccJerkstdTimeX","BodyAccJerkstdTimeY","BodyAccJerkstdTimeZ","BodyGyromeanTimeX","BodyGyromeanTimeY","BodyGyromeanTimeZ","BodyGyrostdTimeX","BodyGyrostdTimeY","BodyGyrostdTimeZ","BodyGyroJerkmeanTimeX","BodyGyroJerkmeanTimeY","BodyGyroJerkmeanTimeZ","BodyGyroJerkstdTimeX","BodyGyroJerkstdTimeY","BodyGyroJerkstdTimeZ","BodyAccMagmean","BodyAccMagstd","GravityAccMagmean","GravityAccMagstd","BodyAccJerkMagmean","BodyAccJerkMagstd","BodyGyroMagmean","BodyGyroMagstd","BodyGyroJerkMagmean","BodyGyroJerkMagstd","BodyAccmeanFreqX","BodyAccmeanFreqY","BodyAccmeanFreqZ","BodyAccstdFreqX","BodyAccstdFreqY","BodyAccstdFreqZ","BodyAccJerkmeanFreqX","BodyAccJerkmeanFreqY","BodyAccJerkmeanFreqZ","BodyAccJerkstdFreqX","BodyAccJerkstdFreqY","BodyAccJerkstdFreqZ","BodyGyromeanFreqX","BodyGyromeanFreqY","BodyGyromeanFreqZ","BodyGyrostdFreqX","BodyGyrostdFreqY","BodyGyrostdFreqZ","BodyAccMagmean","BodyAccMagstd","BodyAccJerkMagmean","BodyAccJerkMagstd","BodyGyroMagmean","BodyGyroMagstd","BodyGyroJerkMagmean","BodyGyroJerkMagstd")
tidydata <- data2

##Step5:Calculating the summary of the data 
tidydatasummary <- aggregate(data2[, names(data2)[-(1:2)]],  list(subject=data2$Subject,Activity = data2$Activity),
                             function(x) c(mean=mean(x), sd=sd(x) ) )


