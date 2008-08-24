! { dg-do compile }

! Parsing of finalizer procedure definitions.
! Check for appropriate errors on invalid final procedures.

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
    FINAL :: finalize_matrix ! { dg-error "must be inside a derived type" }
  CONTAINS
    FINAL :: ! { dg-error "Empty FINAL" }
    FINAL ! { dg-error "Empty FINAL" }
    FINAL :: + ! { dg-error "Expected module procedure name" }
    FINAL :: iamnot ! { dg-error "is not a SUBROUTINE" }
    FINAL :: finalize_single finalize_vector ! { dg-error "Expected ','" }
    FINAL :: finalize_single, finalize_vector
    FINAL :: finalize_single ! { dg-error "is already defined" }
    FINAL :: finalize_vector_2 ! { dg-error "has the same rank" }
    FINAL :: finalize_single_2 ! { dg-error "has the same rank" }
    FINAL :: bad_function ! { dg-error "is not a SUBROUTINE" }
    FINAL bad_num_args_1 ! { dg-error "must have exactly one argument" }
    FINAL bad_num_args_2 ! { dg-error "must have exactly one argument" }
    FINAL bad_arg_type
    FINAL :: bad_pointer
    FINAL :: bad_alloc
    FINAL :: bad_optional
    FINAL :: bad_intent_out

    ! TODO:  Test for polymorphism, kind parameters once those are implemented.
  END TYPE mytype

CONTAINS

  SUBROUTINE finalize_single (el)
    IMPLICIT NONE
    TYPE(mytype) :: el
  END SUBROUTINE finalize_single

  ELEMENTAL SUBROUTINE finalize_single_2 (el)
    IMPLICIT NONE
    TYPE(mytype), INTENT(IN) :: el
  END SUBROUTINE finalize_single_2

  SUBROUTINE finalize_vector (el)
    IMPLICIT NONE
    TYPE(mytype), INTENT(INOUT) :: el(:)
  END SUBROUTINE finalize_vector

  SUBROUTINE finalize_vector_2 (el)
    IMPLICIT NONE
    TYPE(mytype), INTENT(IN) :: el(:)
  END SUBROUTINE finalize_vector_2
  
  SUBROUTINE finalize_matrix (el)
    IMPLICIT NONE
    TYPE(mytype) :: el(:, :)
  END SUBROUTINE finalize_matrix

  INTEGER FUNCTION bad_function (el)
    IMPLICIT NONE
    TYPE(mytype) :: el

    bad_function = 42
  END FUNCTION bad_function

  SUBROUTINE bad_num_args_1 ()
    IMPLICIT NONE
  END SUBROUTINE bad_num_args_1

  SUBROUTINE bad_num_args_2 (el, x)
    IMPLICIT NONE
    TYPE(mytype) :: el
    COMPLEX :: x
  END SUBROUTINE bad_num_args_2

  SUBROUTINE bad_arg_type (el) ! { dg-error "must be of type 'mytype'" }
    IMPLICIT NONE
    REAL :: el
  END SUBROUTINE bad_arg_type

  SUBROUTINE bad_pointer (el) ! { dg-error "must not be a POINTER" }
    IMPLICIT NONE
    TYPE(mytype), POINTER :: el
  END SUBROUTINE bad_pointer

  SUBROUTINE bad_alloc (el) ! { dg-error "must not be ALLOCATABLE" }
    IMPLICIT NONE
    TYPE(mytype), ALLOCATABLE :: el(:)
  END SUBROUTINE bad_alloc

  SUBROUTINE bad_optional (el) ! { dg-error "must not be OPTIONAL" }
    IMPLICIT NONE
    TYPE(mytype), OPTIONAL :: el
  END SUBROUTINE bad_optional

  SUBROUTINE bad_intent_out (el) ! { dg-error "must not be INTENT\\(OUT\\)" }
    IMPLICIT NONE
    TYPE(mytype), INTENT(OUT) :: el
  END SUBROUTINE bad_intent_out

END MODULE final_type

PROGRAM finalizer
  IMPLICIT NONE
  ! Nothing here, errors above
END PROGRAM finalizer

! TODO: Remove this once finalization is implemented.
! { dg-excess-errors "not yet implemented" }

! { dg-final { cleanup-modules "final_type" } }
