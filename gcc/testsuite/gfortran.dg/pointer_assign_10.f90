! { dg-do run }
!
! PR fortran/57530
!
!
! TYPE => TYPE pointer assignment for functions
!
module m
  implicit none
  type t
    integer :: ii = 55
  end type t
contains
  function f1()
    type(t), pointer :: f1
    allocate (f1)
    f1%ii = 123
  end function f1
  function f2()
    type(t), pointer :: f2(:)
    allocate (f2(3))
    f2(:)%ii = [-11,-22,-33]
  end function f2
end module m

program test
  use m
  implicit none
  type(t), pointer :: p1, p2(:), p3(:,:)
  p1 => f1()
  if (p1%ii /= 123) call abort ()
  p2 => f2()
  if (any (p2%ii /= [-11,-22,-33])) call abort ()
  p3(2:2,1:3) => f2()
  if (any (p3(2,:)%ii /= [-11,-22,-33])) call abort ()
end program test
