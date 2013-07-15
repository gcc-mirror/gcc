! { dg-do run }
! { dg-options "-O0" }
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
if (.not.allocated(x%caf)) call abort()
if (.not.allocated(y%caf)) call abort()
if (y%caf /= 6) call abort ()
x = y
if (x%caf /= 6) call abort ()
if (.not. associated (ptr,y%caf)) call abort()
if (associated (ptr,x%caf)) call abort()
ptr = 123
if (y%caf /= 123) call abort ()
if (x%caf /= 6) call abort ()
end program main
