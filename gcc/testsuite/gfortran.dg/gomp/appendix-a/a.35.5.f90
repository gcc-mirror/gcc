! { dg-do compile }

      SUBROUTINE WRONG5(N)
      INTEGER N
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP CRITICAL
            CALL WORK(N,1)
! incorrect nesting of barrier region in a critical region
!$OMP BARRIER
            CALL WORK(N,2)
!$OMP END CRITICAL
!$OMP END PARALLEL
      END SUBROUTINE WRONG5
