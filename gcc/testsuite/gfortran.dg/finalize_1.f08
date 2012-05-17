! { dg-do compile }

! Parsing of finalizer procedure definitions.
! Check that CONTAINS is allowed in TYPE definition; but empty only for F2008

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
  CONTAINS
  END TYPE mytype

CONTAINS
  
  SUBROUTINE bar
    TYPE :: t
    CONTAINS ! This is ok
    END TYPE t
    ! Nothing
  END SUBROUTINE bar

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Do nothing here
END PROGRAM finalizer
