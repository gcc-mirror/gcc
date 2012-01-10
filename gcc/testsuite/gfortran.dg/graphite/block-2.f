      SUBROUTINE MATRIX_MUL_UNROLLED (A, B, C, L, M, N)
      DIMENSION A(L,M), B(M,N), C(L,N)

      DO 100 K = 1, N
        DO 100 I = 1, L
          C(I,K) = 0.
100     CONTINUE
      DO 110 J = 1, M, 4
        DO 110 K = 1, N
          DO 110 I = 1, L
            C(I,K) = C(I,K) + A(I,J) * B(J,K)
     $             + A(I,J+1) * B(J+1,K) + A(I,J+2) * B(J+2,K)
     $             + A(I,J+3) * B(J+3,K)
110   CONTINUE

      RETURN
      END

! { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-times "will be loop blocked" 2 "graphite" { xfail *-*-* } } }
! { dg-final { cleanup-tree-dump "graphite" } }
