! { dg-do compile }
! { dg-options "-std=gnu" }

! Allow legacy code to work even if not only a single symbol is used as
! expression but a basic arithmetic expression.

SUBROUTINE test (n, m)
  IMPLICIT NONE

  ! These should go fine.
  INTEGER :: arr1(n + 1) ! { dg-bogus "used before it is typed" }
  INTEGER :: arr2(n / (2 * m**5)) ! { dg-bogus "used before it is typed" }

  ! These should fail for obvious reasons.
  INTEGER :: arr3(n * 1.1) ! { dg-error "must be of INTEGER type" }
  INTEGER :: arr4(REAL (m)) ! { dg-error "used before it is typed" }
  INTEGER :: arr5(SIN (m)) ! { dg-error "used before it is typed" }

  INTEGER :: n, m
END SUBROUTINE test
