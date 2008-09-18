! { dg-do run }
! { dg-shouldfail "runtime error" }
! { dg-output "At line 13.*Attempting to allocate .* 'arr'" }

! PR fortran/37507
! Check that locus is printed for ALLOCATE errors.

PROGRAM main
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: arr(:)

  ALLOCATE (arr(5))
  ALLOCATE (arr(6))
END PROGRAM main
