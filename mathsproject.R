print("what do want to analyse ?")
print("1=avearge salary vs year")
print("2=max package vs year")
print("3=total placement records")
print("4=placement in each branch in particular year")
print("5=placement  by each company in a particular year")
query = readline(prompt = "enter your option")
if (query==1)
{
  data = read.csv("AVGSALARY.csv" ,header = T  , sep = ",")
  print("what type of analysis?")
  print("1=print data")
  print("2=line chart")
  print("3=height chart")
  print("4=pie chart")
  print("5=bar chart")
  print("6=summary")
  query = readline(prompt = "enter your option")
  if (query==1)
  {
    print(data)
  }else if(query==2)
  {
    plot(data$avgsal,type="l",main="sal vs year",xlab="years",ylab="avg salary",col="blue",xaxt = "n")
    axis(side=1, at=c(1:nrow(data)), labels=data$year)
    
  }else if(query==3)
  {
    plot(data[,1], data[,2], type = "h",main="avg sal vs years",xlab="years",ylab="avg salary")
  }else if(query==4)
  {
    pie(data[,2],labels=data$year,col=c("red","orange","yellow","green","blue","black"),main="avg sal vs year")
  }else if(query==5)
  {
    barplot(data$avgsal,main="avg salary vs year", xlab="years",ylab="avg salary",names.arg = data$year,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,5))
  }else if(query==6)
  {
    summary(data)
  }
}else if (query==2)
{
  data = read.csv("MAXPACKAGE.csv" ,header = T  , sep = ",")
  print("what type of analysis?")
  print("1=print data")
  print("2=line chart")
  print("3=height chart")
  print("4=pie chart")
  print("5=bar chart")
  print("6=summary")
  query = readline(prompt = "enter your option")
  if (query==1)
  {
    print(data)
  }else if(query==2)
  {
    plot(data$maxpack,type="l",main="maxpack vs year",xlab="years",ylab="maxpack",col="blue",xaxt = "n")
    axis(side=1, at=c(1:nrow(data)), labels=data$year)
  }else if(query==3)
  {
    plot(data[,2], data[,1], type = "h",main="maxpack vs years",xlab="years",ylab="maxpack")
  }else if(query==4)
  {
    pie(data[,2],labels=data$year,col=c("red","orange","yellow","green","blue","black"),main="maxpack vs year")
  }else if(query==5)
  {
    barplot(data$maxpack,main="max placement vs year", xlab="years",ylab="maxpack",names.arg = data$year,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,30))
  }else if(query==6)
  {
    summary(data)
  }
}else if (query==3)
{
  data = read.csv("PLACERECORD.csv" ,header = T  , sep = ",")
  print("what type of analysis?")
  print("1=print data")
  print("2=line chart")
  print("3=height chart")
  print("4=pie chart")
  print("5=bar chart")
  print("6=summary")
  query = readline(prompt = "enter your option")
  if (query==1)
  {
    print(data)
  }else if(query==2)
  {
    plot(data$placement,type="l",main="placement vs year",xlab="years",ylab="placement",col="blue",xaxt = "n")
    axis(side=1, at=c(1:nrow(data)), labels=data$year)
  }else if(query==3)
  {
    plot(data[,1], data[,2], type = "h",main="placement vs years",xlab="years",ylab="placement")
  }else if(query==4)
  {
    pie(data[,2],labels=data$year,col=c("red","orange","yellow","green","blue","black"),main="placement vs year")
  }else if(query==5)
  {
    barplot(data$placement,main="placement vs year", xlab="years",ylab="placement",names.arg = data$year,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,5500))
  }else if(query==6)
  {
    summary(data)
  }
}else if(query==4)
{
  print("enter the year of analysis")
  print("1=2012-13")
  print("2=2011-12")
  print("3=2010-11")
  query = readline(prompt = "Enter your option")
  if (query==1)
  {
    data = read.csv("PLACEMENT 12-13.csv" ,header = T  , sep = ",")
    sno = data[,1]
    branch = data[,2]
    placement = data[,3]
    print("what type of analysis?")
    print("1=print data")
    print("2=line chart")
    print("3=height chart")
    print("4=pie chart")
    print("5=bar chart")
    print("6=total student placed")
    print("7=student placed under particular branch")
    print("8=compare two branch")
    print("9=details with boundary value")
    print("10=get max and min")
    query = readline(prompt = "Enter your option ")
    if (query==6)
    {
      print("total student placed")
      print(sum(data$placement))
    }else if(query==7)
    {
      query = readline(prompt = "Enter the branch ? ")
      index = match(query , branch)
      print("Number of students placed")
      x=placement[index]
      print(placement[index])
      s=which(data$placement==x)
      p=sum(data$placement)
      print("total student")
      print(p)
      plotpie<-c(p,x)
      pie(plotpie,col=c("red","orange") ,c("total students","student under branch"))
    }else if(query==1)
    {
      (data)
    }else if(query==2)
    {
      plot(data$placement,type="l",main="placement vs branch",xlab="branch",ylab="placement",col="blue",xaxt = "n")
      axis(side=1, at=c(1:nrow(data)), labels=data$branch)
    }else if(query==3)
    {
      plot(data[,2], data[,3], type = "h",main="placement vs branch",xlab="branch",ylab="placement")
    }else if(query==4)
    {
      pie(data[,3],labels=data$branch,col=c("red","orange","yellow","green","blue","black"),main="placement vs branch")
    }else if(query==5)
    {
      barplot(data$placement,main="placement vs branch", xlab="branch",ylab="placement",names.arg = data$branch,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,50),ylim=c(0,600))
    }else if (query==8)
    {
      branch1= readline(prompt = "Enter the branch 1? ")
      branch2= readline(prompt = "Enter the branch 2? ")
      index1=match(branch1,data[,2])
      data[index1,3]
      index2=match(branch2,data[,2])
      data[index2,3]
      branch<-c(branch1,branch2)
      placement<-c(data[index1,3],data[index2,3])
      compare=data.frame(branch,placement)
      print(compare)
      barplot(compare$placement,main="placement vs year", xlab="branch",ylab="placement",names.arg = compare$branch,col=c("red","blue","yellow","green","orange","black"),xlim=c(0,15),ylim=c(0,100
      ))
    }else if(query==9)
    {
      x=as.integer(readline(prompt = "Enter the boundary value ? "))
      branch<- c()
      placement<-c()
      for (val in unique(data$placement))
      {
        if  (val>=x)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            branch<-c(branch,c(as.character(data[val,2])))
            placement<-c(placement,c(temp))
          }
        }
      }
      boundary=data.frame(branch,placement)
      print(boundary)
      print("no of branch above given boundary")
      print(length(boundary$placement))
      plot(boundary$placement,type="l",main="placement vs branch", ylim=c(0,500),xlab="branch" ,ylab="placement",col="blue", xaxt = "n" )
      axis(side=1, at=c(1:nrow(boundary)), labels=branch)
    }else if(query==10)
    {
      print("the max and min are")
      r<-c(max(data[,3]))
      s<-c(min(data[,3]))
      print(r)
      print(s)
      minmaxvariable<- c()
      minmaxvalue<-c()
      for (val in unique(data$placement))
      {
        if  (val==r|val==s)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            minmaxvariable<-c(minmaxvariable,c(as.character(data[val,2])))
            minmaxvalue<-c(minmaxvalue,c(temp))
          }
        }
      }
      
      maxmin=data.frame(minmaxvariable,minmaxvalue)
      print(maxmin)
      plot(maxmin[,1],maxmin[,2],type = "h",main="max min plot",xlab="company",ylab="Selection",ylim=c(0,r))
    }
  }else if (query==2)
  {
    data = read.csv("PLACEMENT 11-12.csv" ,header = T  , sep = ",")
    sno = data[,1]
    branch = data[,2]
    placement = data[,3]
    print("what type of analysis?")
    print("1=print data")
    print("2=line chart")
    print("3=height chart")
    print("4=pie chart")
    print("5=bar chart")
    print("6=total student placed")
    print("7=student placed under particular branch")
    print("8=compare two branch")
    print("9=details with boundary value")
    print("10=get max and min")
    query = readline(prompt = "Enter your option ")
    if (query==6)
    {
      print("total student placed")
      print(sum(data$placement))
    }else if(query==7)
    {
      query = readline(prompt = "Enter the branch ? ")
      index = match(query , branch)
      print("Number of students placed")
      x=placement[index]
      print(placement[index])
      s=which(data$placement==x)
      p=sum(data$placement)
      print("total student")
      print(p)
      plotpie<-c(p,x)
      pie(plotpie,col=c("red","orange"),c("total students","student under branch"))
    }else if(query==1)
    {
      print(data)
    }else if(query==2)
    {
      plot(data$placement,type="l",main="placement vs branch",xlab="branch",ylab="placement",col="blue",xaxt = "n")
      axis(side=1, at=c(1:nrow(data)), labels=data$branch)
    }else if(query==3)
    {
      plot(data[,2], data[,3], type = "h",main="placement vs branch",xlab="branch",ylab="placement")
    }else if(query==4)
    {
      pie(data[,3],labels=data$branch,col=c("red","orange","yellow","green","blue","black"),main="placement vs branch")
    }else if(query==5)
    {
      barplot(data$placement,main="placement vs branch", xlab="branch",ylab="placement",names.arg = data$branch,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,50),ylim=c(0,50))
    }else if (query==8)
    {
      branch1= readline(prompt = "Enter the branch 1? ")
      branch2= readline(prompt = "Enter the branch 2? ")
      index1=match(branch1,data[,2])
      data[index1,3]
      index2=match(branch2,data[,2])
      data[index2,3]
      branch<-c(branch1,branch2)
      placement<-c(data[index1,3],data[index2,3])
      compare=data.frame(branch,placement)
      print(compare)
      barplot(compare$placement,main="placement vs year", xlab="branch",ylab="placement",names.arg = compare$branch,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,300))
      
    }else if(query==9)
    {
      x=as.integer(readline(prompt = "Enter the boundary value ? "))
      
      branch<- c()
      placement<-c()
      for (val in unique(data$placement))
      {
        if  (val>=x)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            branch<-c(branch,c(as.character(data[val,2])))
            placement<-c(placement,c(temp))
          }
        }
      }
      boundary=data.frame(branch,placement)
      print(boundary)
      print("no of branch above given boundary")
      print(length(boundary$branch))
      plot(boundary$placement,type="l",main="placement vs branch", ylim=c(0,500),xlab="branch" ,ylab="placement",col="blue", xaxt = "n" )
      axis(side=1, at=c(1:nrow(boundary)), labels=branch)
    }else if(query==10)
    {
      print("the max and min are")
      r<-c(max(data[,3]))
      s<-c(min(data[,3]))
      print(r)
      print(s)
      minmaxvariable<- c()
      minmaxvalue<-c()
      for (val in unique(data$placement))
      {
        if  (val==r|val==s)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            minmaxvariable<-c(minmaxvariable,c(as.character(data[val,2])))
            minmaxvalue<-c(minmaxvalue,c(temp))
          }
        }
      }
      
      maxmin=data.frame(minmaxvariable,minmaxvalue)
      print(maxmin)
      plot(maxmin[,1],maxmin[,2],type = "h",main="max min plot",xlab="company",ylab="Selection",ylim=c(0,r))
    }
  }else if (query==3)
  {
    data = read.csv("PLACEMENT 10-11.csv" ,header = T  , sep = ",")
    sno = data[,1]
    branch = data[,2]
    placement = data[,3]
    print("what type of analysis?")
    print("1=print data")
    print("2=line chart")
    print("3=height chart")
    print("4=pie chart")
    print("5=bar chart")
    print("6=total student placed")
    print("7=student placed under particular branch")
    print("8=compare two branch")
    print("9=details with boundary value")
    print("10=get max and min")
    query = readline(prompt = "Enter your option ")
    if (query==6)
    {
      print("total student placed")
      print(sum(data$placement))
    }else if(query==7)
    {
      query = readline(prompt = "Enter the branch ? ")
      index = match(query , branch)
      print("Number of students placed")
      x=placement[index]
      print(placement[index])
      s=which(data$placement==x)
      p=sum(data$placement)
      print("total student")
      print(p)
      plotpie<-c(p,x)
      pie(plotpie,col=c("red","orange"),c("total students","student under branch"))
    }else if(query==1)
    {
      print(data)
    }else if(query==2)
    {
      plot(data$placement,type="l",main="placement vs branch",xlab="branch",ylab="placement",col="blue",xaxt = "n")
      axis(side=1, at=c(1:nrow(data)), labels=data$branch)
    }else if(query==3)
    {
      plot(data[,2], data[,3], type = "h",main="placement vs branch",xlab="branch",ylab="placement")
    }else if(query==4)
    {
      pie(data[,3],labels=data$branch,col=c("red","orange","yellow","green","blue","black"),main="placement vs branch")
    }else if(query==5)
    {
      barplot(data$placement,main="placement vs branch", xlab="branch",ylab="placement",names.arg = data$branch,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,50),ylim=c(0,50))
    }else if (query==8)
    {
      branch1= readline(prompt = "Enter the branch 1? ")
      branch2= readline(prompt = "Enter the branch 2? ")
      index1=match(branch1,data[,2])
      data[index1,3]
      index2=match(branch2,data[,2])
      data[index2,3]
      branch<-c(branch1,branch2)
      placement<-c(data[index1,3],data[index2,3])
      compare=data.frame(branch,placement)
      print(compare)
      barplot(compare$placement,main="placement vs year", xlab="branch",ylab="placement",names.arg = compare$branch,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,15),ylim=c(0,300))
      
    }else if(query==9)
    {
      x=as.integer(readline(prompt = "Enter the boundary value ? "))
      
      branch<- c()
      placement<-c()
      for (val in unique(data$placement))
      {
        if  (val>=x)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            branch<-c(branch,c(as.character(data[val,2])))
            placement<-c(placement,c(temp))
          }
        }
      }
      boundary=data.frame(branch,placement)
      print(boundary)
      print("no of branch above given boundary")
      print(length(boundary$placement))
      plot(boundary$placement,type="l",main="placement vs branch", ylim=c(0,500),xlab="branch" ,ylab="placement",col="blue", xaxt = "n" )
      axis(side=1, at=c(1:nrow(boundary)), labels=branch)
    }else if(query==10)
    {
      print("the max and min are")
      r<-c(max(data[,3]))
      s<-c(min(data[,3]))
      print(r)
      print(s)
      minmaxvariable<- c()
      minmaxvalue<-c()
      for (val in unique(data$placement))
      {
        if  (val==r|val==s)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            minmaxvariable<-c(minmaxvariable,c(as.character(data[val,2])))
            minmaxvalue<-c(minmaxvalue,c(temp))
          }
        }
      }
      
      maxmin=data.frame(minmaxvariable,minmaxvalue)
      print(maxmin)
      plot(maxmin[,1],maxmin[,2],type = "h",main="max min plot",xlab="company",ylab="Selection",ylim=c(0,r))
    }
  }
}else if (query==5)
{
  print("enter the year of analysis")
  print("1=2012-13")
  print("2=2011-12")
  query = readline(prompt = "Enter your option")
  if (query==1)
  {
    data = read.csv("SELECTIONS 12-13.csv" ,header = T  , sep = ",")
    sno = data[,1]
    company = data[,2]
    Selections = data[,3]
    print("what type of analysis?")
    print("1=print data")
    print("2=line chart")
    print("3=height chart")
    print("4=pie chart")
    print("5=bar chart")
    print("6=total student selected")
    print("7=student placed under particular company")
    print("8=compare two company")
    print("9=details with boundary value")
    print("10=get max and min")
    query = readline(prompt = "Enter your option ")
    if (query==6)
    {
      print("total student placed")
      print(sum(data$Selections))
    }else if(query==7)
    {
      query = readline(prompt = "Enter the company ? ")
      index = match(query ,company )
      print("Number of students selected")
      x=Selections[index]
      print(Selections[index])
      s=which(data$Selections==x)
      p=sum(data$Selections)
      print("total student")
      print(p)
      plotpie<-c(p,x)
      pie(plotpie,col=c("red","orange") ,c("total students","student under company"))
    }else if(query==1)
    {
      print(data)
    }else if(query==2)
    {
      plot(data$Selections,type="l",main="Selections vs company",xlab="company",ylab="Selections",ylim= c(0,100),col="blue",xaxt = "n")
      axis(side=1, at=c(1:nrow(data)), labels=data$company)
    }else if(query==3)
    {
      plot(data[,2], data[,3], type = "h",main="Selections vs company",xlab="company",ylab="Selections",ylim=c(0,20))
    }else if(query==4)
    {
      pie(data[,3],labels=data$company,col=c("red","orange","yellow","green","blue","black"),main="Selections vs company")
    }else if(query==5)
    {
      barplot(data$Selections,main="Selections vs year", xlab="company",ylab="Selections",names.arg = data$company,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,50),ylim=c(0,50))
    }else if (query==8)
    {
      company1= readline(prompt = "Enter the company 1? ")
      company2= readline(prompt = "Enter the company 2? ")
      index1=match(company1,data[,2])
      data[index1,3]
      index2=match(company2,data[,2])
      data[index2,3]
      company<-c(company1,company2)
      Selections<-c(data[index1,3],data[index2,3])
      compare=data.frame(company,Selections)
      print(compare)
      barplot(compare$Selections,main="Selections vs year", xlab="company",ylab="Selections",names.arg = compare$company,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,50))
      
    }else if(query==9)
    {
      x=as.integer(readline(prompt = "Enter the boundary value ? "))
      
      company<- c()
      Selections<-c()
      for (val in unique(data$Selections))
      {
        if  (val>=x)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            company<-c(company,c(as.character(data[val,2])))
            Selections<-c(Selections,c(temp))
          }
        }
      }
      print("No of company above given boundary")
      
      boundary=data.frame(company, Selections)
      print(boundary)
      print("no of branch above given boundary")
      print(length(boundary$Selections))
      plot(boundary$Selections,type="l",main="Selections vs company", ylim=c(0,100),xlab="company" ,ylab="Selections",col="blue", xaxt = "n" )
      axis(side=1, at=c(1:nrow(boundary)), labels=company)
    }else if(query==10)
    {
      print("the max and min are")
      r<-c(max(data[,3]))
      s<-c(min(data[,3]))
      print(r)
      print(s)
      minmaxvariable<- c()
      minmaxvalue<-c()
      for (val in unique(data$Selections))
      {
        if  (val==r|val==s)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            minmaxvariable<-c(minmaxvariable,c(as.character(data[val,2])))
            minmaxvalue<-c(minmaxvalue,c(temp))
          }
        }
      }
      
      maxmin=data.frame(minmaxvariable,minmaxvalue)
      print(maxmin)
      plot(maxmin[,1],maxmin[,2],type = "h",main="max min plot",xlab="company",ylab="Selection",ylim=c(0,r))
    }
  }else if (query==2)
  {
    data = read.csv("SELECTIONS 11-12.csv" ,header = T  , sep = ",")
    sno=data[,1]
    company=data[,2]
    Selections=data[,3]
    print("what type of analysis?")
    print("1=print data")
    print("2=line chart")
    print("3=height chart")
    print("4=pie chart")
    print("5=bar chart")
    print("6=total student selected")
    print("7=student selected under particular company")
    print("8=compare two company")
    print("9=details with boundary value")
    print("10=get max and min")
    query = readline(prompt = "Enter your option ")
    if (query==6)
    {
      print("total student selected")
      print(sum(data$Selections))
    }else if(query==7)
    {
      query = readline(prompt = "Enter the company ? ")
      index = match(query , company)
      print("Number of students selected")
      x=Selections[index]
      print(Selections[index])
      s=which(data$Selections==x)
      p=sum(data$Selections)
      print("total student")
      print(p)
      plotpie<-c(p,x)
      pie(plotpie,col=c("red","orange") ,c("total students","student under company"))
    }else if(query==1)
    {
      print(data)
    }else if(query==2)
    {
      plot(data$Selections,type="l",main="Selections vs company",xlab="company",ylab="Selections",col="blue",xaxt = "n",ylim=c(0,200))
      axis(side=1, at=c(1:nrow(data)), labels=data$company)
    }else if(query==3)
    {
      plot(data[,2], data[,3], type = "h",main="Selections vs company",xlab="company",ylab="Selections",ylim=c(0,20))
    }else if(query==4)
    {
      pie(data[,3],labels=data$company,col=c("red","orange","yellow","green","blue","black"),main="Selections vs company")
    }else if(query==5)
    {
      barplot(data$Selections,main="Selections vs year", xlab="company",ylab="Selections",names.arg = data$company,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,50),ylim=c(0,50))
    }else if (query==8)
    {
      company1= readline(prompt = "Enter the company 1? ")
      company2= readline(prompt = "Enter the company 2? ")
      index1=match(company1,data[,2])
      data[index1,3]
      index2=match(company2,data[,2])
      data[index2,3]
      company<-c(company1,company2)
      Selections<-c(data[index1,3],data[index2,3])
      compare=data.frame(company,Selections)
      print(compare)
      barplot(compare$Selections,main="Selections vs year", xlab="company",ylab="Selections",names.arg = compare$company,col=c("red","orange","yellow","green","blue","black"),xlim=c(0,12),ylim=c(0,50))
      
    }else if(query==9)
    {
      x=as.integer(readline(prompt = "Enter the boundary value ? "))
      
      company<- c()
      Selections<-c()
      for (val in unique(data$Selections))
      {
        if  (val>=x)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            company<-c(company,c(as.character(data[val,2])))
            Selections<-c(Selections,c(temp))
          }
        }
      }
      
      boundary=data.frame(company,Selections)
      print(boundary)
      print("no of company above given boundary")
      print(length(boundary$Selections))
      plot(boundary$Selections,type="l",main="Selections vs company", ylim=c(0,100),xlab="company" ,ylab="Selections",col="blue", xaxt = "n" )
      axis(side=1, at=c(1:nrow(boundary)), labels=company)
    }else if(query==10)
    {
      print("the max and min are")
      r<-c(max(data[,3]))
      s<-c(min(data[,3]))
      print(r)
      print(s)
      minmaxvariable<- c()
      minmaxvalue<-c()
      for (val in unique(data$Selections))
      {
        if  (val==r|val==s)
        {
          
          temp<-val
          indices <-val ==data[,3]
          a<-c(which(a<-indices))
          for (val in a)
          {
            
            
            minmaxvariable<-c(minmaxvariable,c(as.character(data[val,2])))
            minmaxvalue<-c(minmaxvalue,c(temp))
          }
        }
      }
      3
      
      maxmin=data.frame(minmaxvariable,minmaxvalue)
      print(maxmin)
      plot(maxmin[,1],maxmin[,2],type = "h",main="max min plot",xlab="company",ylab="Selection",ylim=c(0,r))
    }
  }
} 



