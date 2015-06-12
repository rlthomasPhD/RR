# RR
Assignment1
library(knitr)

#Loading and preprocessing the data
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", 
                                                    "numeric"))
summary(activity)
head(activity)
names(activity)

#Mean number of steps taken per day
steps.day <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.day$steps, names.arg = steps.day$date, xlab = "date", ylab = "steps")

mean(steps.day$steps)

#Average daily activity pattern
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

steps.interval <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.interval, type = "l")
steps.interval$interval[which.max(steps.interval$steps)]

#Imputing missing values
sum(is.na(activity))

activity <- merge(activity, steps.interval, by = "interval", suffixes = c("", 
                                                                          ".y"))
na <- is.na(activity$steps)
activity$steps[na] <- activity$steps.y[na]
activity <- activity[, c(1:3)]

steps.day <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(steps.day$steps, names.arg = steps.day$date, xlab = "date", ylab = "steps")

#Differences between Weekday and weekend activity patterns
day <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$day <- as.factor(sapply(activity$date, day))
head(activity$day)

#Graph
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$day == 
                            type, FUN = mean)
  plot(steps.type, type = "l", main = type)
}






