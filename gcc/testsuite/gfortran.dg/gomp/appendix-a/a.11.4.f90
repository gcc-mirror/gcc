! { dg-do compile }

      SUBROUTINE A11_4(AA, BB, CC, DD, EE, FF, GG, HH, N)
      INTEGER N
      REAL AA(N,N), BB(N,N), CC(N,N)
      REAL DD(N,N), EE(N,N), FF(N,N)
      REAL GG(N,N), HH(N,N)
!$OMP PARALLEL
!$OMP WORKSHARE
            AA = BB
            CC = DD
            WHERE (EE .ne. 0) FF = 1 / EE
            GG = HH
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_4
