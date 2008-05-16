! { dg-do run }
! PR fortran/27997
!
! Nested array constructors with typespec.

PROGRAM test
  IMPLICIT NONE

  INTEGER(KIND=8) :: arr(3)
  CHARACTER(len=6) :: carr(3)

  arr = (/ INTEGER(KIND=8) :: 4, [ INTEGER(KIND=4) :: 42, 12 ] /)
  IF (arr(1) /= 4 .OR. arr(2) /= 42 .OR. arr(3) /= 12) CALL abort()
  arr = (/ INTEGER(KIND=8) :: [ INTEGER(KIND=4) :: 4, 42, 12 ] /)
  IF (arr(1) /= 4 .OR. arr(2) /= 42 .OR. arr(3) /= 12) CALL abort()
  arr = (/ INTEGER(KIND=8) :: [ INTEGER(KIND=4) :: 4, 42 ], 12 /)
  IF (arr(1) /= 4 .OR. arr(2) /= 42 .OR. arr(3) /= 12) CALL abort()
  arr = (/ INTEGER(KIND=8) :: [ INTEGER(KIND=4) :: ], 4, 42, 12 /)
  IF (arr(1) /= 4 .OR. arr(2) /= 42 .OR. arr(3) /= 12) CALL abort()

  carr = [ CHARACTER(len=6) :: "foo", [ CHARACTER(len=4) :: "foobar", "xyz" ] ]
  IF (carr(1) /= "foo" .OR. carr(2) /= "foob" .OR. carr(3) /= "xyz") THEN
    CALL abort()
  END IF
END PROGRAM test
