! { dg-do compile }
! { dg-options "-std=f95" }

! Test for a special case of the used-before-typed errors, when the symbols
! not-yet-typed are indices.

SUBROUTINE test (n, arr1, m, arr2) ! { dg-error "has no IMPLICIT type" }
  IMPLICIT NONE

  INTEGER :: myarr(42)

  INTEGER :: arr1(SIZE (myarr(1:n))) ! { dg-error "'n' is used before" }
  INTEGER :: n

  INTEGER :: arr2(LEN ("hello"(1:m))) ! { dg-error "'m' is used before" }
  INTEGER :: m

  WRITE (*,*) SIZE (arr1)
  WRITE (*,*) SIZE (arr2)
END SUBROUTINE test

PROGRAM main
  IMPLICIT NONE
  INTEGER :: arr1(42), arr2(42)
  CALL test (3, arr1, 2, arr2)
END PROGRAM main
