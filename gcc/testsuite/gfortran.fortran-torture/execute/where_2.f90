! Program to test the WHERE constructs
program where_2
   integer temp(10), reduce(10)
   
   temp = 10
   reduce(1:3) = -1 
   reduce(4:6) = 0
   reduce(7:8) = 5 
   reduce(9:10) = 10

   WHERE (reduce < 0) 
      temp = 100 
   ELSE WHERE (reduce .EQ. 0)
      temp = 200 + temp
   ELSE WHERE 
      WHERE (reduce > 6) temp = temp + sum(reduce)
      temp = 300 + temp
   END WHERE
   
   if (any (temp .ne. (/100, 100, 100, 210, 210, 210, 310, 310, 337, 337/))) &
      call abort
end program
