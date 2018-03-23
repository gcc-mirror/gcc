! { dg-do compile }
! { dg-additional-options "-ffrontend-optimize" }
! PR 84133 - this used to ICE. Original test case by
! Gerhard Steinmetz.

program p
   real :: x(2,2) = 1.0
   real :: z(2,2)
   associate (y => matmul(x,x))
      z = y
   end associate
   print *, z
end

  
