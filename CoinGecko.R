# CoinGecko parser ----------

# Part 1 - getting list of cryptolinks
url<-"https://www.coingecko.com/en/coins/all"
url2<-"https://www.coingecko.com/en/coins/"

web_page <- readLines(url)

indx1<-grep("/en/coins/",web_page)
indx2<-grep("a href",web_page)
indx3<-grep("/en/coins/all",web_page)
indx4<-grep("/trading_exchanges",web_page)
indx<-setdiff(intersect(intersect(indx1,indx2),indx4),indx3)
web_page[indx[2000:2087]]
cryptonames<-web_page[indx]
w<-strsplit(cryptonames,">")

x<-1
name<-1
ticker<-1
tick<-1
for (i in 1:length(w)){
  x[i]<-as.vector(strsplit(w[[i]][2],"[(]"))
  name[i]<-x[[i]][1]
  tick[i]<-as.vector(strsplit(x[[i]][2],"[)]"))
  ticker[i]<-tick[[i]][1]
}
df<-as.data.frame(cbind(name,ticker))[-1,]
name<-trimws(name, which = c("right"))
rm(cryptonames,i,indx,w,web_page,x,name,tick,ticker)
df$name<-trimws(df$name, which = c("right"))
df$cname<-gsub(" ","-",tolower(df$name))
df$link<-paste(url2,df$cname,sep="")
df2<-df
df<-df[1:1061,]
# Part 2 --------- collecting info
clue<-"<div class=score"
clue<-gsub("score",'["]score["]',clue)
x<-1
name<-1
x2<-1
score<-1
df$total<-200
df$liquidity<-200
df$developer<-200
df$community<-200
for (crypto in 1:1061){
  
  temp_page <- readLines(df[crypto,4])
  a <- grep(clue, temp_page)
  if (length(a)>0) {
    t<-unique(temp_page[a])
    w<-strsplit(t,">")
    
    for (i in 1:length(w)){
      
      x2[i]<-as.vector(strsplit(w[[i]][3],"[<]"))
      score[i]<-x2[[i]][1]
      df[crypto,5]<-score[1]
      df[crypto,6]<-score[2]
      df[crypto,7]<-score[3]
      df[crypto,8]<-score[4]
     
    }
    print(crypto)
  } 
  
}
str(df)
df$total<-as.numeric(gsub("%","",df$total))
df$liquidity<-as.numeric(gsub("%","",df$liquidity))
df$community<-as.numeric(gsub("%","",df$community))
df$developer<-as.numeric(gsub("%","",df$developer))

Sys.Date()
filename<-paste("key_index_as_of_",Sys.Date(),sep="",".csv")
setwd("/home/alexander/Рабочий стол/CoinGecko")
write.csv(df,file=filename,row.names = F)
