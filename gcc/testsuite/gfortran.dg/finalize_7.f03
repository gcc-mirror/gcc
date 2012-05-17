! { dg-do compile }
! { dg-options "-Wsurprising" }

! Implementation of finalizer procedures.
! Check for expected warnings on dubious FINAL constructs.

MODULE final_type
  IMPLICIT NONE

  TYPE :: type_1
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
  CONTAINS
    ! Non-scalar procedures should be assumed shape
    FINAL :: fin1_scalar
    FINAL :: fin1_shape_1
    FINAL :: fin1_shape_2
  END TYPE type_1

  TYPE :: type_2 ! { dg-warning "Only array FINAL procedures" }
    REAL :: x
  CONTAINS
    ! No scalar finalizer, only array ones
    FINAL :: fin2_vector
  END TYPE type_2

CONTAINS

  SUBROUTINE fin1_scalar (el)
    IMPLICIT NONE
    TYPE(type_1) :: el
  END SUBROUTINE fin1_scalar

  SUBROUTINE fin1_shape_1 (v) ! { dg-warning "assumed shape" }
    IMPLICIT NONE
    TYPE(type_1) :: v(*)
  END SUBROUTINE fin1_shape_1

  SUBROUTINE fin1_shape_2 (v) ! { dg-warning "assumed shape" }
    IMPLICIT NONE
    TYPE(type_1) :: v(42, 5)
  END SUBROUTINE fin1_shape_2

  SUBROUTINE fin2_vector (v)
    IMPLICIT NONE
    TYPE(type_2) :: v(:)
  END SUBROUTINE fin2_vector

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Nothing here
END PROGRAM finalizer

! TODO: Remove this once finalization is implemented.
! { dg-excess-errors "not yet implemented" }
