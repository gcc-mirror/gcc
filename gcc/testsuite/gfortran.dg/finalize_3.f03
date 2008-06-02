! { dg-do compile }

! Parsing of finalizer procedure definitions.
! Check that CONTAINS disallows further components and no double CONTAINS
! is allowed.

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
  CONTAINS
  CONTAINS ! { dg-error "Already inside a CONTAINS block" }
    INTEGER :: x ! { dg-error "must precede CONTAINS" }
  END TYPE mytype

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Do nothing here
END PROGRAM finalizer

! { dg-final { cleanup-modules "final_type" } }
