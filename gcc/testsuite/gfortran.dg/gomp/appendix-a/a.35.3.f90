! { dg-do compile }

      SUBROUTINE WRONG3(N)
      INTEGER N
        INTEGER I
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO
          DO I = 1, N
!$OMP SINGLE            ! incorrect nesting of regions
               CALL WORK(I, 1)
!$OMP END SINGLE
          END DO
!$OMP END PARALLEL
      END SUBROUTINE WRONG3
