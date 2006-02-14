! { dg-do compile }
      SUBROUTINE A11_7(AA, BB, CC, N)
      INTEGER N
      REAL AA(N), BB(N), CC(N)
!$OMP PARALLEL
!$OMP WORKSHARE
            AA(1:50) = BB(11:60)
            CC(11:20) = AA(1:10)
!$OMP END WORKSHARE
!$OMP END PARALLEL
      END SUBROUTINE A11_7
