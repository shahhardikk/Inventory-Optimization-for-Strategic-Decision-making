# Part1 
# 3) Implement model  
demand_annual <- 10800
cost_per_unit <- 78
opp_cost <- 0.19
supplier_cost <- 168

mathematical_function <- function(order_quantity){
  order_amount <- demand_annual/order_quantity
  
  annual_ordering_cost <- supplier_cost * order_amount 
  annual_holding_cost <- order_quantity/2 * cost_per_unit * opp_cost
  
  total_cost <- annual_holding_cost + annual_ordering_cost
}

# 4) data tables 
order_quantity <- seq(200,5000,by=1)
total_cost <- mathematical_function(order_quantity)
result1 <- data.frame(order_quantity, total_cost)
result1

# 5) plot 
plot(order_quantity, total_cost, 
     pch=5, 
     las=1, 
     main="Quantity & Total cost",
     xlab="Quantity", 
     ylab="Total cost",
     ylim = c(5000,40000))
lines(order_quantity, total_cost)

# 6) verify result 
mathematical_Interval <- c(100, 700)
mathematical_Opt <- optimise(f=mathematical_function, interval=mathematical_Interval,
                             upper=max(mathematical_Interval),
                             lower=min(mathematical_Interval),
                             maximum=FALSE)
mathematical_Opt



# Part2
# 1) simulation (combine all three parts together)
mathematical_simulation <- function(a,b,c){

  K <- (c-a)/(b-a)
  M <- (b-a)*(c-a)
  N <- (b-a)*(b-c)
  
  total_cost <<- c()
  order_quantity <<- c()
  order_amount <<- c()
  
  n <- 3000
  random <- runif(n)
  
  for (i in 1:3000){
    if (random[i] <= K){
      demand_annual <<- a+sqrt(M*random[i])
    }else{
      demand_annual <<- b-sqrt(N*(1-random[i]))
    }
    
    
    mathematical_Interval <- c(100, 700)
    mathematical_Opt <- optimise(f=mathematical_function, interval=mathematical_Interval,
                                 upper=max(mathematical_Interval),
                                 lower=min(mathematical_Interval),
                                 maximum=FALSE)
    
    
    total_cost[i] <<- mathematical_Opt[[2]]
    order_quantity[i] <<- mathematical_Opt[[1]]
    order_amount[i] <<- demand_annual/order_quantity[i]
  }
}



# 95% CI level
confidence_95<- function(x){
  mean <- mean(x)
  std <- sd(x)
  margin <- qnorm(0.975)*std/sqrt(length(x))
  left_end <- mean - margin
  right_end <- mean + margin
  
  interval <- c(mean, left_end, right_end)
  print(interval)
}

a <- 8000
c <- 12000 
b <- 10000
mathematical_simulation(a,b,c)

confidence_95(total_cost)
confidence_95(order_quantity)
confidence_95(order_amount)

# plot 
hist(total_cost, 
     freq=F,
     breaks=30)
lines(density(total_cost), 
      lwd=2,
      col="RED")

hist(order_quantity, 
     freq=F)
lines(density(order_quantity), 
      lwd=2,
      col="BLUE",
      ylim=c(0.00,0.40))

hist(order_amount, 
     freq=F,
     breaks=30)
lines(density(order_amount), 
      lwd=2,
      col="green")
