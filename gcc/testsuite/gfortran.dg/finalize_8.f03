! { dg-do compile }

! Parsing of finalizer procedure definitions.
! Check that FINAL-declarations are only allowed on types defined in the
! specification part of a module.

MODULE final_type
  IMPLICIT NONE

CONTAINS

  SUBROUTINE bar
    IMPLICIT NONE

    TYPE :: mytype
      INTEGER, ALLOCATABLE :: fooarr(:)
      REAL :: foobar
    CONTAINS
      FINAL :: myfinal ! { dg-error "in the specification part of a MODULE" }
    END TYPE mytype

  CONTAINS

    SUBROUTINE myfinal (el)
      TYPE(mytype) :: el
    END SUBROUTINE myfinal

  END SUBROUTINE bar

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Do nothing here
END PROGRAM finalizer
