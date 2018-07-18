! { dg-do run }
! { dg-options "-std=f2003  -fcheck=bounds" }

! PR fortran/45016
! Check pointer bounds remapping at runtime.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, TARGET :: arr(2_2:5), basem(-2:-1, 3:4_1)
  INTEGER, POINTER :: vec(:), vec2(:), mat(:, :)

  arr = (/ 1, 2, 3, 4 /)
  basem = RESHAPE (arr, SHAPE (basem))

  vec(0:) => arr
  IF (LBOUND (vec, 1) /= 0 .OR. UBOUND (vec, 1) /= 3) STOP 1
  IF (ANY (vec /= arr)) STOP 2
  IF (vec(0) /= 1 .OR. vec(2) /= 3) STOP 3

  ! Test with bound different of index type, so conversion is necessary.
  vec2(-5_1:) => vec
  IF (LBOUND (vec2, 1) /= -5 .OR. UBOUND (vec2, 1) /= -2) STOP 4
  IF (ANY (vec2 /= arr)) STOP 5
  IF (vec2(-5) /= 1 .OR. vec2(-3) /= 3) STOP 6

  mat(1:, 2:) => basem
  IF (ANY (LBOUND (mat) /= (/ 1, 2 /) .OR. UBOUND (mat) /= (/ 2, 3 /))) &
    STOP 7
  IF (ANY (mat /= basem)) STOP 8
  IF (mat(1, 2) /= 1 .OR. mat(1, 3) /= 3 .OR. mat(2, 3) /= 4) STOP 9
END PROGRAM main
