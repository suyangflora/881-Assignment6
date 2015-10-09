library(ggplot2)
library(qualityTools)


# create a vector of w exponential waiting times with lambda = lam

wait <- function(w,lam){
  set.seed(50)
  a = NULL
  for(i in 1:w){
    a = c(a,rexp(1,rate = lam))   # output with each waiting time with exponential distribution
  }
  return(a)
}

# try it with w=3, lambda=2, supposed there should be three waiting times

wait(3,2)
# I set three waiting times and each time we wait at a average of exponential of 1/2
# for each waiting time we got: 0.21£¬0.42£¬0.01



# create a vector of exponential waiting times which total t <= Max with lambda = lam

wait.until <- function(Max,lam){
  set.seed(50)
  time = 0
  a = NULL
  while(time < Max){    # for time within max limits,output is each waiting time with exponential distribution.
    inter = rexp(1,lam)
    a = c(a,inter)
    time = time + inter
  }
  return(a[1:(length(a)-1)])  
}

# test by max = 10, lambda =2
wait.until(10,2)
# interpret: in t=10, for every waiting time we have exponentially rate of 2, number of event might be around 20.

# check whether it fit the maximum limits.
sum(wait.until(10,2))
sum(wait.until(11,2))
## when max = 11, sum is larger than 10. So the results show that the function is correct.



## Words from Haviland
# now simulate the number of events to show that the number of events divided by
# exponential waiting times are Poisson distributed
# (don't forget to comment out the "set.seed")

## My words 
# if events happens at a rate of ¦Ë per unit time, first event happen time is distributed exponentially with rate ¦Ë
# if wait for t units of time, the number of events happen in this time is Poisson(¦Ët) random variable.
# sum of independent exponential distributed rv follows gamma(¦Ë,n)

poi.test <- function(rep, Max, lam){
  set.seed(50)
  a = NULL
  for(i in 1:rep){
    q = wait.until(Max,lam)
    a = c(a,length(q))
  }
  return(a)
}
poi.test(3,20,2)

# interpret: repeat waiting procedure for 3 times, calculate number of events
# each time we have t=20 and each waiting time exponentially distributed at rate of 2.
# as result, we have "35,35,35" all are around 20/(1/2)=40 which seems reasonable.



# now simlate the waiting time for k events to occur with lambda = lam

wait.for <- function(k, lam){
  set.seed(50)
  time = 0
  count = 0
  a = NULL
  while(count < k){
    inter=rexp(1,lam)   # internal waiting time between two events happen.
    count = count + 1
    time = time+inter
  }
  
  return(time)
} 

# with exponential rate of 2, time when first event happens.
wait.for(1,2)


gam.test <-function(rep, max.e, lam ){
  set.seed(50)
  a=NULL
  for (i in 1:rep){
    t = wait.for(max.e,lam)    # max.e means number of events occur.
    a = c(a,t)
    
  }
  
  return(a)
}
  
# repeat for two times to calculate total waiting time for 3 events to happen.  
gam.test(2,3,2)  
  
  
  
  

