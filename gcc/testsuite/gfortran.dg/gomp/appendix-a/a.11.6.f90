! { dg-do compile }

      SUBROUTINE A11_6_WRONG(AA, BB, CC, DD, N)
      INTEGER N
      REAL AA(N,N), BB(N,N), CC(N,N), DD(N,N)
        INTEGER PRI
!$OMP PARALLEL PRIVATE(PRI)
!$OMP WORKSHARE
            AA = BB
            PRI = 1
            CC = DD * PRI
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_6_WRONG
