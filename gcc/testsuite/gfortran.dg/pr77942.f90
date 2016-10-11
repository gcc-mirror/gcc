! { dg-do compile }
program p
   character, parameter :: c(2) = 'a'
   print *, cshift(c(2:1), 1)
end
