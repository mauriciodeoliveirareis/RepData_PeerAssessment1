# Reproducible Research: Peer Assessment 1

## Non standard R libs used
For the plots, I've used ggplot2 lib install.packages("ggplot2")
require(ggplot2)
TODO put this in code with control of is installed


## Loading and preprocessing the data
I've cloned the git repository https://github.com/rdpeng/RepData_PeerAssessment1 in the commit   80edf39c3bb508fee88e3394542f967dd3fd3270 with the data and instructions for this assignment.    
This cloned repository contains the data that I used in this assignment, it's a zip file called activity.zip that  
has inside a csv file called activity.csv, the following code extracts the activity.csv file from activity.zip.

```r
unzip("activity.zip")
```

I read the csv file to R and observed it's dimensions and summary


```r
activityDf <- read.csv("activity.csv")
dim(activityDf)
```

```
## [1] 17568     3
```

```r
summary(activityDf)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```


## What is mean total number of steps taken per day?
Here I just ignored the missing values in the dataset, for that I created a new data.frame just with entries without NAs:  

```r
activityDfNoNa <- activityDf[!is.na(activityDf$steps), ]
```
Then I've made some work with the data without NAs.

```r
totalSteps <- sum(activityDfNoNa$steps)
totalDays <- length(levels(activityDf$date))
```
The indivitual of this experiment has taken 570608 steps during the 61 days that he was wearing his personal activity monitoring device.

I've calculated the steps per day ignoring the days that have NA valuesm generated a histogram and calculated its mean and median:

```r
stepsPerDay <- tapply(activityDfNoNa$steps, activityDfNoNa$date, FUN = sum)
stepsPerDay <- stepsPerDay[!is.na(stepsPerDay)]
stepsPerDayMean <- format(round(mean(stepsPerDay), 2), nsmall = 2)
stepsPerDayMedian <- format(round(median(stepsPerDay), 2), nsmall = 2)
hist(stepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


The mean of steps per day here is 10766.19 and the median is 10765.00

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
