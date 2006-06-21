! { dg-do run }
program m
  integer i
  real x,y
  real, parameter :: a = -3.0
  i = int(rrspacing(a))
  if (i /= 12582912) call abort
end program m
