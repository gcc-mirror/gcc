! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=lib" }
!
! PR fortran/52052
!
! Test that for CAF components _gfortran_caf_deregister is called
! Test that norealloc happens for CAF components during assignment
!
module m
type t
  integer, allocatable :: CAF[:]
  integer, allocatable :: ii
end type t
end module m

subroutine foo()
use m
type(t) :: x,y
if (allocated(x%caf)) call abort()
x = y
end

! For comp%ii: End of scope of x + y (2x) and for the LHS of the assignment (1x)
! { dg-final { scan-tree-dump-times "__builtin_free" 3 "original" } }

! For comp%CAF:  End of scope of x + y (2x); no LHS freeing for the CAF in assignment
! { dg-final { scan-tree-dump-times "_gfortran_caf_deregister" 2 "original" } }

! Only malloc "ii":
! { dg-final { scan-tree-dump-times "__builtin_malloc" 1 "original" } }

! But copy "ii" and "CAF":
! { dg-final { scan-tree-dump-times "__builtin_memcpy" 2 "original" } }

! { dg-final { cleanup-tree-dump "original" } }
