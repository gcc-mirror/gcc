! { dg-do compile }

       SUBROUTINE A31_2 (A, B, X, Y, N)
         INTEGER N
         REAL X(*), Y(*), A, B, A_P, B_P
!$OMP PARALLEL SHARED(X, Y, N, A, B) PRIVATE(A_P, B_P)
        A_P = 0.0
        B_P = HUGE(B_P)
!$OMP DO PRIVATE(I)
        DO I=1,N
          A_P = A_P + X(I)
          B_P = MIN(B_P, Y(I))
        ENDDO
!$OMP END DO
!$OMP CRITICAL
          A = A + A_P
          B = MIN(B, B_P)
!$OMP END CRITICAL
!$OMP END PARALLEL
      END SUBROUTINE A31_2
