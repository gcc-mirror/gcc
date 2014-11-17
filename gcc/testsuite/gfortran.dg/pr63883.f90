! { dg-do compile }
! { dg-additional-options "-ffast-math" }
  SUBROUTINE influence_factor ( gftype, error )
    INTEGER, PARAMETER :: dp=8
    INTEGER :: k,n,lb(3),ub(3),dim,pt
    COMPLEX(KIND=dp)                         :: b_m, exp_m, sum_m
    DO k = 0, n-2
       DO pt = lb (dim), ub (dim)
          sum_m = CMPLX ( 0.0_dp, 0.0_dp,KIND=dp)
          b_m = exp_m ** ( n - 1 ) / sum_m
       END DO
    END DO
  END SUBROUTINE influence_factor
