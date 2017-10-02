! { dg-options "-O2 -fgraphite-identity -fno-loop-block -fno-loop-interchange -fno-loop-strip-mine -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" }

MODULE beta_gamma_psi
  INTEGER, PARAMETER :: dp=KIND(0.0D0)
CONTAINS
  FUNCTION basym (a, b, lambda, eps) RESULT(fn_val)
    REAL(dp) :: a0(21), b0(21), bsum, c(21), d(21), dsum, &
         j0, j1, r, r0, r1, s, sum, t, t0, t1, &
         u, w, w0, z, z0, z2, zn, znm1
    DO n = 2, num, 2
       DO i = n, np1
          b0(1) = r*a0(1)
          DO m = 2, i
             bsum = 0.0e0_dp
             mm1 = m - 1
             DO j = 1, mm1
                mmj = m - j
                bsum = bsum + (j*r - mmj)*a0(j)*b0(mmj)
             END DO
             b0(m) = r*a0(m) + bsum/m
          END DO
          c(i) = b0(i)/(i + 1.0e0_dp)
          d(i) = -(dsum + c(i))
       END DO
       t0 = d(n)*w*j0
       sum = sum + (t0 + t1)
    END DO
    fn_val = e0*t*u*sum
  END FUNCTION basym
END MODULE beta_gamma_psi

! { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } }
