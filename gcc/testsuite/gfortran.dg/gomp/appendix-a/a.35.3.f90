! { dg-do compile }

      SUBROUTINE WRONG3(N)
      INTEGER N
        INTEGER I
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO
          DO I = 1, N
	       ! incorrect nesting of regions
!$OMP SINGLE   ! { dg-error "may not be closely nested" }
               CALL WORK(I, 1)
!$OMP END SINGLE
          END DO
!$OMP END PARALLEL
      END SUBROUTINE WRONG3
