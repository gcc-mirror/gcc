! { dg-do run }

! PR fortran/36592
!
! Procedure Pointers inside COMMON blocks.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>.

subroutine one()
  implicit none
  common /com/ p1,p2,a,b
  procedure(real), pointer :: p1,p2
  integer :: a,b
  if (a/=5 .or. b/=-9 .or. p1(0.0)/=1.0 .or. p2(0.0)/=0.0) call abort()
end subroutine one

program main
  implicit none
  integer :: x,y
  intrinsic sin,cos
  procedure(real), pointer :: func1
  real, external :: func2
  pointer func2
  common /com/ func1,func2,x,y
  x = 5
  y = -9
  func1 => cos
  func2 => sin
  call one()
end program main

