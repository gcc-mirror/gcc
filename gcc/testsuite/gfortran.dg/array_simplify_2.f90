! { dg-do  run }
! PR 85102 - this used to ICE
! Original test case by Gerhard Steinmetz
program p
   integer, parameter :: a((1+2)) = 1
   integer, parameter :: b((1+1)+1) = 1
   integer, parameter :: c = dot_product(a, a)
   integer, parameter :: d = dot_product(b,b)
   if (c /= 3) stop 1
   if (d /= 3) stop 2
 end program p
