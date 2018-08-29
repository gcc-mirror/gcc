! { dg-do run }
!
! Test that for CAF components _gfortran_caf_deregister is called
! Test that norealloc happens for CAF components during assignment
!
module m
type t
  integer, allocatable :: CAF[:]
end type t
end module m

program main
use m
type(t), target :: x,y
integer, pointer :: ptr
allocate(x%caf[*], y%caf[*])
ptr => y%caf
ptr = 6
if (.not.allocated(x%caf)) STOP 1
if (.not.allocated(y%caf)) STOP 2
if (y%caf /= 6) STOP 3
x = y
if (x%caf /= 6) STOP 4
if (.not. associated (ptr,y%caf)) STOP 5
if (associated (ptr,x%caf)) STOP 6
ptr = 123
if (y%caf /= 123) STOP 7
if (x%caf /= 6) STOP 8
end program main
