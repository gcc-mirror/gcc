! { dg-do run }
! { dg-shouldfail "runtime error" }
! { dg-output "At line 14.*Attempt to DEALLOCATE unallocated 'arr'" }

! PR fortran/37507
! Check that locus is printed for DEALLOCATE errors.

PROGRAM main
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: arr(:)

  ALLOCATE (arr(5))
  DEALLOCATE (arr)
  DEALLOCATE (arr)
END PROGRAM main
