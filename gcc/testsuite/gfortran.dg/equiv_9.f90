! { dg-do run }
! PR fortran/66377
!
module constant
  integer x1, x2, x3
  integer x(3)
  equivalence (x(1),x1), (x2,x(2)), (x3,x(3))
end module

program test
  use constant
  implicit none 
  x = (/1, 2, 3/)
  call another()
end program

subroutine another()
   use constant, only : x2
   implicit none
   if (x2 /= 2) STOP 1
end subroutine
