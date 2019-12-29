library(dplyr)

dat = read.csv("activity.csv")
# dat <- filter(dat, !is.na(steps))
dat <- mutate(dat, date = factor(as.Date(date, "%Y-%m-%d")))
dat <- mutate(dat, interval = factor(interval))

gDate <- group_by(dat, date)

# 1. Calculate the total number of steps taken per day
sDate <- summarise(gDate, total = sum(steps, na.rm = TRUE))

# 2. Make a histogram of the total number of steps taken each day
plot(sDate$date, sDate$total, type = "S")
hist(sDate$total, breaks = 20)
# 3. Calculate and report the mean and median of the 
#   total number of steps taken per day
meanDate <- mean(sDate$total)
medianDate <- median(sDate$total)

# 1. Make a time series plot of 5-min interval (x-axis)
#   and avg no of steps taken, avg across all days (y-axis)
gInterval <- group_by(dat, interval)
sInterval <- summarise(gInterval, avgStep = mean(steps, na.rm = TRUE))
meanStep <- mean(sInterval$avgStep)
plot(sInterval$interval, sInterval$avgStep, type = "1")

# 2. Which 5-minute interval, on average across all the days in the 
#   dataset, contains the maximum number of steps?
sInterval$interval[sInterval$avgStep == max(sInterval$avgStep)]

# 1. Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
sum(is.na(dat$steps))

# 2&3. Replace NA with mean for that 5-min interval
dat = read.csv("activity.csv")
#dat = dat[200:500,]
dat <- mutate(dat, date = factor(as.Date(date, "%Y-%m-%d")))
dat <- mutate(dat, interval = factor(interval))

gDate <- group_by(dat, date)
sDate <- summarise(gDate, total = sum(steps, na.rm = TRUE))

plot(sDate$date, sDate$total, type = "S")
hist(sDate$total, breaks = 20)
meanDate <- mean(sDate$total)
medianDate <- median(sDate$total)

# 1. Create a new factor variable in the dataset with two levels – 
#   “weekday” and “weekend” 
dat <- mutate(dat, day = weekdays(as.Date(date, format = "%Y-%m-%d")))
table(dat$day)
#dat <- mutate(dat, weekend = if (day=="Saturday") TRUE else FALSE)
#table(dat$weekend)
for (i in 1:nrow(dat)){
  if(dat[i,4] == "Saturday" | dat[i,4] == "Sunday" ){
    dat[i,4] = "weekend"
    print(dat[i,4])
  }
  else{
    dat[i,4] = "weekday"
  }
}
dat <- mutate(dat, day = factor(day))

dat.weekday <- filter(dat, day=="weekday")
dat.weekend <- filter(dat, day=="weekend")

par(mfcol = c(2, 1))
gInterval.weekend <- group_by(dat.weekend, interval)
sInterval.weekend <- summarise(gInterval.weekend, avgStep = mean(steps, na.rm = TRUE))
plot(sInterval.weekend$interval, sInterval.weekend$avgStep, type = "1",
     main="weekend", xlab="Interval", ylab="Number of Step")
gInterval.weekday <- group_by(dat.weekday, interval)
sInterval.weekday <- summarise(gInterval.weekday, avgStep = mean(steps, na.rm = TRUE))
plot(sInterval.weekday$interval, sInterval.weekday$avgStep, type = "1",
     main="weekday", xlab="Interval", ylab="Number of Step")

