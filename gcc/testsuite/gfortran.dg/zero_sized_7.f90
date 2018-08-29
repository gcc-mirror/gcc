! { dg-do compile }
! PR 80118 - this used to ICE
! Original test case by Marco Restelli
module m
implicit none

  integer, parameter :: not_empty(1) = 0
  integer, parameter :: empty1(0) = (/integer :: /)
  integer, parameter :: empty2(0) = 0

contains

 subroutine sub(v)
  integer, allocatable, intent(out) :: v(:)
   v = 2*empty2 ! internal compiler error
 end subroutine sub

end module m
