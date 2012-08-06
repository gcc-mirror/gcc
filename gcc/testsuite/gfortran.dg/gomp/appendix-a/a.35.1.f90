! { dg-do compile }

      SUBROUTINE WORK(I, J)
      INTEGER I, J
      END SUBROUTINE WORK
      SUBROUTINE WRONG1(N)
      INTEGER N
        INTEGER I,J
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO
          DO I = 1, N
	     ! incorrect nesting of loop regions
!$OMP DO     ! { dg-warning "may not be closely nested" }
             DO J = 1, N
                CALL WORK(I,J)
             END DO
          END DO
!$OMP END PARALLEL
      END SUBROUTINE WRONG1
