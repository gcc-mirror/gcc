! { dg-options "-O2 -fgraphite-identity -fno-loop-block -fno-loop-interchange -fno-loop-strip-mine" }

MODULE beta_gamma_psi
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
CONTAINS
  FUNCTION basym () RESULT(fn_val)
    REAL(dp) :: b0(21), bsum, d(21)
    DO n = 2, num, 2
       DO i = n, np1
          b0(1) = 1
          DO m = 2, i
             mm1 = m - 1
             DO j = 1, mm1
                bsum = bsum + b0(j)
             END DO
             b0(m) = bsum
          END DO
          d(i) = -b0(i)
       END DO
       sum = sum + d(n)
    END DO
    fn_val = sum
  END FUNCTION basym
END MODULE beta_gamma_psi
