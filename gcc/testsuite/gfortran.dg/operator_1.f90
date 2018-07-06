! { dg-do run }
! Test the extension of intrinsic operators
module m1
 interface operator(*)
  module procedure f1
  module procedure f2
  module procedure f3
 end interface

 interface operator(.or.)
  module procedure g1
 end interface

 interface operator(//)
  module procedure g1
 end interface

contains

 function f1(a,b) result (c)
  integer, dimension(2,2), intent(in) :: a
  integer, dimension(2), intent(in)   :: b
  integer, dimension(2)   :: c
  c = matmul(a,b)
 end function f1
 function f2(a,b) result (c)
  real, dimension(2,2), intent(in) :: a
  real, dimension(2), intent(in)   :: b
  real, dimension(2)   :: c
  c = matmul(a,b)
 end function f2
 function f3(a,b) result (c)
  complex, dimension(2,2), intent(in) :: a
  complex, dimension(2), intent(in)   :: b
  complex, dimension(2)   :: c
  c = matmul(a,b)
 end function f3

 elemental function g1(a,b) result (c)
   integer, intent(in) :: a, b
   integer :: c
   c = a + b
 end function g1

end module m1

  use m1
  implicit none

  integer, dimension(2,2) :: ai
  integer, dimension(2)   :: bi, ci
  real, dimension(2,2) :: ar
  real, dimension(2)   :: br, cr
  complex, dimension(2,2) :: ac
  complex, dimension(2)   :: bc, cc

  ai = reshape((/-2,-4,7,8/),(/2,2/)) ; bi = 3
  if (any((ai*bi) /= matmul(ai,bi))) STOP 1
  if (any((ai .or. ai) /= ai+ai)) STOP 2
  if (any((ai // ai) /= ai+ai)) STOP 3

  ar = reshape((/-2,-4,7,8/),(/2,2/)) ; br = 3
  if (any((ar*br) /= matmul(ar,br))) STOP 4

  ac = reshape((/-2,-4,7,8/),(/2,2/)) ; bc = 3
  if (any((ac*bc) /= matmul(ac,bc))) STOP 5

end
