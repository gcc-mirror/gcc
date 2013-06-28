! { dg-do compile }

! Parsing of finalizer procedure definitions.
! Check parsing of valid finalizer definitions.

MODULE final_type
  IMPLICIT NONE

  TYPE :: mytype
    INTEGER, ALLOCATABLE :: fooarr(:)
    REAL :: foobar
  CONTAINS
    FINAL :: finalize_single
    FINAL finalize_vector, finalize_matrix
    ! TODO:  Test with different kind type parameters once they are implemented.
  END TYPE mytype

CONTAINS

  ELEMENTAL SUBROUTINE finalize_single (el)
    IMPLICIT NONE
    TYPE(mytype), INTENT(IN) :: el
    ! Do nothing in this test
  END SUBROUTINE finalize_single

  SUBROUTINE finalize_vector (el)
    IMPLICIT NONE
    TYPE(mytype), INTENT(INOUT) :: el(:)
    ! Do nothing in this test
  END SUBROUTINE finalize_vector

  SUBROUTINE finalize_matrix (el)
    IMPLICIT NONE
    TYPE(mytype) :: el(:, :)
    ! Do nothing in this test
  END SUBROUTINE finalize_matrix

END MODULE final_type

PROGRAM finalizer
  USE final_type, ONLY: mytype
  IMPLICIT NONE

  TYPE(mytype) :: el, vec(42)
  TYPE(mytype), ALLOCATABLE :: mat(:, :)

  ALLOCATE(mat(2, 3))
  DEALLOCATE(mat)

END PROGRAM finalizer
