! { dg-do run }
!
! PR fortran/39304
!
! matmul checking was checking the wrong specific function
! ("one" instead of "two")
!
module m
  implicit none
  interface one
    module procedure one, two
  end interface one
contains
  function one()
    real :: one(1)
    one = 0.0
  end function one
  function two(x)
    real :: x
    real :: two(1,1)
    two = reshape ( (/ x /), (/ 1, 1 /) )
  end function two
end module m

use m
real :: res(1)
res = matmul (one(2.0), (/ 2.0/))
if (abs (res(1)-4.0) > epsilon (res)) call abort ()
end
