C PR fortran/24493
C { dg-do compile }
C { dg-require-effective-target tls_native }
      INTEGER I, J, K, L, M
C$OMP THREADPRIVATE(I)
C SOME COMMENT
      SAVE I ! ANOTHER COMMENT
C$OMP THREADPRIVATE
C$OMP+(J) ! OMP DIRECTIVE COMMENT
* NORMAL COMMENT
c$OMP THREAD! COMMENT
C$OMP&PRIVATE! COMMENT
*$OMP+    (K)
C$OMP THREADPRIVATE (L ! COMMENT
*$OMP& , M)
      SAVE J, K, L, M
      I = 1
      J = 2
      K = 3
      L = 4
      M = 5
      END
