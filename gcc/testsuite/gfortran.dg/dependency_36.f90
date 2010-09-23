! { dg-do compile }
! { dg-options "-O -Warray-temporaries" }
! PR 45744 - this used to ICE because of type mismatch
!            in the generated temporary.
MODULE m
CONTAINS
  FUNCTION rnd(n)
    INTEGER, INTENT(in) :: n
    REAL(8), DIMENSION(n) :: rnd
    CALL RANDOM_NUMBER(rnd)
  END FUNCTION rnd

  SUBROUTINE GeneticOptimize(n)
    INTEGER :: n
    LOGICAL :: mask(n)
    REAL(8) :: popcross=0
    REAL(4) :: foo(n)
    real(4) :: a(n,n), b(n,n)
    real(8) :: c(n,n)
    integer(4) :: x(n,n)
    integer(8) :: bar(n)
    mask = (rnd(n) < popcross)  ! { dg-warning "Creating array temporary" }
    foo = rnd(n)                ! { dg-warning "Creating array temporary" }
    bar = rnd(n)                ! { dg-warning "Creating array temporary" }
    c = matmul(a,b)             ! { dg-warning "Creating array temporary" }
    x = matmul(a,b)             ! { dg-warning "Creating array temporary" }
  END SUBROUTINE GeneticOptimize
END MODULE m
