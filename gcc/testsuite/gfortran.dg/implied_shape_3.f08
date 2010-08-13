! { dg-do compile }
! { dg-options "-std=f2008" }

! Test for errors with implied-shape declarations.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  INTEGER :: n
  INTEGER, PARAMETER :: mat(2, 2) = RESHAPE ((/ 1, 2, 3, 4 /), (/ 2, 2 /))

  ! Malformed declaration.
  INTEGER, PARAMETER :: arr1(*, *, 5) = mat ! { dg-error "Bad array specification for implied-shape array" }

  ! Rank mismatch in initialization.
  INTEGER, PARAMETER :: arr2(*, *) = (/ 1, 2, 3, 4 /) ! { dg-error "Incompatible ranks" }

  ! Non-PARAMETER implied-shape, with and without initializer.
  INTEGER :: arr3(*, *) ! { dg-error "Non-PARAMETER" }
  INTEGER :: arr4(*, *) = mat ! { dg-error "Non-PARAMETER" }

  ! Missing initializer.
  INTEGER, PARAMETER :: arr5(*) ! { dg-error "is missing an initializer" }

  ! Initialization from scalar.
  INTEGER, PARAMETER :: arr6(*) = 0 ! { dg-error "with scalar" }

  ! Automatic bounds.
  n = 2
  BLOCK
    INTEGER, PARAMETER :: arr7(n:*) = (/ 2, 3, 4 /) ! { dg-error "Non-constant lower bound" }
  END BLOCK
END PROGRAM main
