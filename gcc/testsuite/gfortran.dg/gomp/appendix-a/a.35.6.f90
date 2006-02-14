! { dg-do compile }

         SUBROUTINE WRONG6(N)
         INTEGER N
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP SINGLE
           CALL WORK(N,1)
! incorrect nesting of barrier region in a single region
!$OMP BARRIER
            CALL WORK(N,2)
!$OMP END SINGLE
!$OMP END PARALLEL
      END SUBROUTINE WRONG6

