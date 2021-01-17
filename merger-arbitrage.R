#Insert data
target = read.csv('/Users/marco/Downloads/ESRX.csv') #Express Scripts
acquirer = read.csv('/Users/marco/Downloads/acquirer.csv') #Cigna

########### DEAL SPECIFICATIONS ##########
#Cigna Corporation (NYSE: CI) and Express Scripts Holding Company (NASDAQ: ESRX) 
#today announced that they have entered into a definitive agreement whereby Cigna will acquire 
#Express Scripts in a cash and stock transaction valued at approximately $67 billion, 
#including Cigna's assumption of approximately $15 billion in Express Scripts debt. 
#The merger consideration will consist of $48.75 in cash and 0.2434 shares of stock of the combined 
#company per Express Scripts share. The transaction was approved by the board of directors of each company.
#$54 billion in the aggregate. Upon closing of the transaction, Cigna shareholders will own 
#approximately 64% of the combined company and Express Scripts shareholders will own approximately 36%. 
#The consideration represents an approximately 31% premium to Express Scripts’ closing price of $73.42 
#on March 7, 2018.
#########################################

#Define index (5 is the announcement date)
index = c(1:nrow(target))

target_price = target$Price
acquirer_price = acquirer$Price

par(mfrow=c(2,1))
plot(index, target_price, type = "l", lty = 1, col = "blue", ylab = "Target")
plot(index, acquirer_price, type = "l", lty = 1, col = "red", ylab = "Acquirer")

#Creating a unique database for calculation
data = as.data.frame(cbind(index, target_price, acquirer_price))
conversion_rate = 0.2434 #one stock of target for 0.2434 acquirer stock

######## ARBITRAGE OPPORTUNITY FOR THE STOCK CONVERSION ########
#We go long target, short 0.2434 acquirer to immediate profit at t = 6 (stock operation)
return_long = (data$target_price[6] - data$target_price[5])/data$target_price[5]
return_short = -(data$acquirer_price[6] - data$acquirer_price[5])/data$acquirer_price[5]
  
total_return = return_long + return_short #total return for an arbitrage at today
  
results = c(return_long, return_short, total_return)
names(results) = c("Returns Long", "Returns Short", "Total Return")

######## ARBITRAGE OPPORTUNITY FOR CASH DEAL ########
#Cash strategy: Long the target until the premium is reached
premium = 0.31 #announced by the acquirer
price_cap = target_price[5]*(1+premium) #we stop when this price is reached

upside = price_cap - target_price[6]
upside_pctg = upside/target_price[6]

downside = target_price[6] - target_price[5]
downside_pctg = downside/target_price[5]

prob_success = downside/(upside+downside) #probability of deal success
deal_break = 1 - prob_success #probability of deal break

deal_info = c(upside, upside_pctg, downside, downside_pctg, deal_break) #summarize in one line
names(deal_info) = c("Upside", "Upside %", "Downside", "Downside %", "Deal Break Prob")

#Define the long position during time and setting condition until the long is closed
long = list()
for (i in 5:length(index)) {

  long[i] = data$target_price[i]
  
    if (data$target_price[i] > price_cap){
    
      break
    
    }
  
  print(long)
  
}

long = as.data.frame(unlist(long))

#Calculate returns and cumulate them for the total return of the strategy (long)
returns = list()
for (i in 2:nrow(long)){
  
  returns[[i]] = long$`unlist(long)`[i]/long$`unlist(long)`[i-1] - 1
  
}

returns = as.data.frame(unlist(returns))
cum_returns = cumsum(returns$`unlist(returns)`)
plot(cum_returns, type = "l", ylab = "Cash Strategy Returns", col = "blue")

#We conclude by summarizing the results of the strategy
return_strategy = c(total_return, cum_returns[length(cum_returns)], total_return + cum_returns[length(cum_returns)])
names(return_strategy) = c("Stock Returns", "Cash Returns", "Total Returns")



