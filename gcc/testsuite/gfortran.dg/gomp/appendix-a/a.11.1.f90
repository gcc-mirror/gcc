! { dg-do compile }
      SUBROUTINE A11_1(AA, BB, CC, DD, EE, FF, N)
      INTEGER N
      REAL AA(N,N), BB(N,N), CC(N,N), DD(N,N), EE(N,N), FF(N,N)
!$OMP PARALLEL
!$OMP WORKSHARE
            AA = BB
            CC = DD
            EE = FF
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_1
