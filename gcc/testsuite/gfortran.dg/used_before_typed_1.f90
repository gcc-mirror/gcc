! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/32095
! PR fortran/34228
! Check that standards-conforming mode rejects uses of variables that
! are used before they are typed.

SUBROUTINE test1 (n, arr, m, arr2, k, arr3, a) ! { dg-error "has no IMPLICIT" }
  IMPLICIT NONE

  INTEGER :: arr(n) ! { dg-error "used before it is typed" }
  INTEGER :: n
  INTEGER :: m, arr2(m) ! { dg-bogus "used before it is typed" }
  INTEGER, DIMENSION(k) :: arr3 ! { dg-error "used before it is typed" }
  INTEGER :: k
  CHARACTER(len=LEN(a)) :: a ! { dg-error "'a' is used before it is typed" }

  REAL(KIND=l) :: x ! { dg-error "has no IMPLICIT type" }
  REAL(KIND=KIND(y)) :: y ! { dg-error "has no IMPLICIT type" }

  DATA str/'abc'/ ! { dg-error "used before it is typed" }
  CHARACTER(len=3) :: str, str2
  DATA str2/'abc'/ ! { dg-bogus "used before it is typed" }
END SUBROUTINE test1

SUBROUTINE test2 (n, arr, m, arr2)
  IMPLICIT INTEGER(a-z)

  INTEGER :: arr(n)
  REAL :: n ! { dg-error "already has basic type" }
  INTEGER :: m, arr2(m) ! { dg-bogus "already has an IMPLICIT type" }
END SUBROUTINE test2

SUBROUTINE test3 (n, arr, m, arr2)
  IMPLICIT REAL(a-z)

  INTEGER :: arr(n) ! { dg-error "must be of INTEGER type" }
  INTEGER :: m, arr2(m) ! { dg-bogus "must be of INTEGER type" }
END SUBROUTINE test3
