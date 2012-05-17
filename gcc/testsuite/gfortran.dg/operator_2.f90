! { dg-do compile }
! Test that we can't override intrinsic operators in invalid ways
module foo

 interface operator(*)
  module procedure f1 ! { dg-error "conflicts with intrinsic interface" }
 end interface

 interface operator(>)
   module procedure f2 ! { dg-error "conflicts with intrinsic interface" }
 end interface

 interface operator(/)
  module procedure f3
 end interface

contains

 function f1(a,b) result (c)
  integer, intent(in) :: a
  integer, dimension(:), intent(in)   :: b
  integer, dimension(size(b,1))   :: c
  c = 0
 end function f1

 function f2(a,b)
   character(len=*), intent(in) :: a
   character(len=*), intent(in) :: b
   logical :: f2
   f2 = .false.
 end function f2

 function f3(a,b) result (c)
  integer, dimension(:,:), intent(in) :: a
  integer, dimension(:), intent(in)   :: b
  integer, dimension(size(b,1))   :: c
  c = 0
 end function f3

end
