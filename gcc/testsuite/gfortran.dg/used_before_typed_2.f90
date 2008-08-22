! { dg-do compile }
! { dg-options "-std=gnu" }

! PR fortran/32095
! PR fortran/34228
! This program used to segfault, check this is fixed.
! Also check that -std=gnu behaves as expected.

SUBROUTINE test1 (n, arr)
  IMPLICIT NONE

  INTEGER :: arr(n) ! { dg-bogus "used before it is typed" }
  INTEGER :: n
  CHARACTER(len=LEN(a)) :: a ! { dg-error "used before it is typed" }
END SUBROUTINE test1

SUBROUTINE test2 ()
  IMPLICIT NONE

  DATA str/'abc'/ ! { dg-bogus "used before it is typed" }
  CHARACTER(len=3) :: str
END SUBROUTINE test2
