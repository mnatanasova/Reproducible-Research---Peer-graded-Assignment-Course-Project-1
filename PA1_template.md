# Reproducible Research: Peer Assessment 1

```{r}

library(ggplot2)
library(chron)
```

## Loading and preprocessing the data
##### 1. Load the data (i.e. read.csv()) and transform the data (if necessary)

```{r}
data <- read.csv("activity.csv", sep = ",")
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day? For this part of the assignment, you can ignore the missing values in the dataset.
##### 1. Calculate the total number of steps taken per day
##### 2. Make a histogram of the total number of steps taken each day
##### 3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
subdata <- data[is.na(data$steps) == FALSE,]

stepsByDay <- aggregate(steps ~ date, subdata, FUN=sum)
hist(stepsByDay$steps,  
     xlab = "Steps", 
     main = "Total Number Of Steps Taken Each day")
stepsByDayMean <- mean(stepsByDay$steps)
stepsByDayMedian <- median(stepsByDay$steps)
stepsByDayMean
stepsByDayMedian	
```
![plot of chunk pic1](/pic1.png)

* mean 10766.19
* median 10765


## What is the average daily activity pattern?
##### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
##### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps? 

```{r}
IntervalSteps <- aggregate(steps ~ interval, data, FUN=sum)
plot(IntervalSteps$interval, IntervalSteps$steps, 
     type = "l", lwd = 2,
     xlab = "Interval", 
     ylab = "Total Steps",
     main = "Total Steps vs. 5-Minute Interval")
	 
IntervalSteps[IntervalSteps$steps == max(IntervalSteps$steps),]
```


## Imputing missing values
##### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
##### 2. Devise a strategy for filling in all of the missing values in the dataset.
##### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
##### 4.1. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
##### 4.2. Do these values differ from the estimates from the first part of the assignment?
##### 4.3. What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
table(is.na(data))
datanew <- data
datanew$steps <- ifelse(is.na(datanew$steps) == TRUE,  mean(datanew$steps, na.rm = TRUE), datanew$steps)

stepsByDaynew <- aggregate(steps ~ date, datanew, FUN=sum)

hist(stepsByDaynew$steps,  
     xlab = "Steps", 
     main = "Total Number Of Steps Taken Each day")
stepsByDayMeannew <- mean(stepsByDaynew$steps)
stepsByDayMediannew <- median(stepsByDaynew$steps)
```

## Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
#### 2. Make a panel plot containing a time series plot

```{r}
datanew$dayofweek <- ifelse(is.weekend(datanew$date), "weekend", "weekday")
meanintervalnew <- aggregate(steps ~ interval + dayofweek, datanew, FUN=mean)
ggplot(meanintervalnew, aes(x=interval, y=steps)) + 
      geom_line(color="blue", size=1) + 
      facet_wrap(~dayofweek, nrow=2) +
      labs(x="\nInterval", y="\nNumber of steps")
```
