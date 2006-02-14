! { dg-do compile }
        SUBROUTINE A11_5(AA, BB, CC, DD, N)
        INTEGER N
        REAL AA(N,N), BB(N,N), CC(N,N), DD(N,N)
        INTEGER SHR
!$OMP PARALLEL SHARED(SHR)
!$OMP WORKSHARE
            AA = BB
            SHR = 1
            CC = DD * SHR
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_5

