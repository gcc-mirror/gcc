! { dg-do compile }
      SUBROUTINE A11_3(AA, BB, CC, DD, N)
      INTEGER N
      REAL AA(N,N), BB(N,N), CC(N,N), DD(N,N)
      REAL R
        R=0
!$OMP PARALLEL
!$OMP WORKSHARE
             AA = BB
!$OMP ATOMIC
               R = R + SUM(AA)
             CC = DD
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_3
