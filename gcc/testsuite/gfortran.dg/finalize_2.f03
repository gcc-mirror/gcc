! { dg-do compile }
! { dg-options "-std=f2003" }

! Parsing of finalizer procedure definitions.
! Check empty CONTAINS errors out for F2003.

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
  CONTAINS
  END TYPE mytype ! { dg-error "Fortran 2008" }

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Do nothing here
END PROGRAM finalizer
