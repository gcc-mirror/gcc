! { dg-do compile }

! PR fortran/35723
! Check that a dummy-argument array with non-restricted subscript is
! rejected and some more reference-checks.

PROGRAM main
  IMPLICIT NONE
  CALL test (5, (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), "0123456789" )

CONTAINS

  SUBROUTINE test (n, arr, str)
    IMPLICIT NONE
    INTEGER :: n, arr(:)
    CHARACTER(len=10) :: str

    INTEGER :: i = 5
    INTEGER :: ok1(arr(n)), ok2(LEN_TRIM (str(3:n)))
    INTEGER :: ok3(LEN_TRIM("hello, world!"(2:n)))
    INTEGER :: wrong1(arr(i)) ! { dg-error "'i' cannot appear" }
    INTEGER :: wrong2(LEN_TRIM (str(i:n))) ! { dg-error "'i' cannot appear" }
    INTEGER :: wrong3(LEN_TRIM ("hello, world!"(i:n))) ! { dg-error "'i' cannot appear" }
  END SUBROUTINE test

END PROGRAM main
