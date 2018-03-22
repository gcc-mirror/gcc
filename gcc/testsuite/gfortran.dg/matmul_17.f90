! { dg-do run }
! PR Fortran/83900
! Contributed by Gerhard Steinmetz  <gscfq t t-online dot de>
program p
   integer, parameter :: a(3,2) = 1
   real, parameter :: b(2,3) = 2
   real, parameter :: c(3,3) = matmul(a, b)
   if (any(c /= 4.)) STOP 1
end
