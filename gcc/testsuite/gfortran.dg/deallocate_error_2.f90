! { dg-do run }
! { dg-shouldfail "runtime error" }
! { dg-output "At line 15.*Attempt to DEALLOCATE unallocated 'ptr'" }

! PR fortran/37507
! Check that locus is printed for DEALLOCATE errors.

PROGRAM main
  IMPLICIT NONE
  INTEGER, POINTER :: ptr
  INTEGER, ALLOCATABLE :: arr(:)

  ALLOCATE (ptr, arr(5))
  DEALLOCATE (ptr)
  DEALLOCATE (arr, ptr)
END PROGRAM main
