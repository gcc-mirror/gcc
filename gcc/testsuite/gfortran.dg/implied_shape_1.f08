! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! Test for correct semantics of implied-shape arrays.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  INTEGER, PARAMETER :: n = 3

  ! Should be able to reduce complex expressions.
  REAL, PARAMETER :: arr1(n:*) = SQRT ((/ 1.0, 2.0, 3.0 /)) + 42

  ! With dimension statement.
  REAL, DIMENSION(*), PARAMETER :: arr2 = arr1

  ! Rank > 1.
  INTEGER, PARAMETER :: arr3(n:*, *) = RESHAPE ((/ 1, 2, 3, 4 /), (/ 2, 2/))

  ! Character array.
  CHARACTER(LEN=*), PARAMETER :: arr4(*) = (/ CHARACTER(LEN=3) :: "ab", "cde" /)

  IF (LBOUND (arr1, 1) /= n .OR. UBOUND (arr1, 1) /= n + 2) CALL abort ()
  IF (SIZE (arr1) /= 3) CALL abort ()

  IF (LBOUND (arr2, 1) /= 1 .OR. UBOUND (arr2, 1) /= 3) CALL abort ()
  IF (SIZE (arr2) /= 3) CALL abort ()

  IF (ANY (LBOUND (arr3) /= (/ n, 1 /) .OR. UBOUND (arr3) /= (/ n + 1, 2 /))) &
    CALL abort ()
  IF (SIZE (arr3) /= 4) CALL abort ()

  IF (LBOUND (arr4, 1) /= 1 .OR. UBOUND (arr4, 1) /= 2) CALL abort ()
  IF (SIZE (arr4) /= 2) CALL abort ()
END PROGRAM main
