! { dg-do compile }

      SUBROUTINE WRONG4(N)
      INTEGER N
        INTEGER I
!$OMP PARALLEL DEFAULT(SHARED)
!$OMP DO
          DO I = 1, N
             CALL WORK(I, 1)
! incorrect nesting of barrier region in a loop region
!$OMP BARRIER	! { dg-warning "may not be closely nested" }
             CALL WORK(I, 2)
          END DO
!$OMP END PARALLEL
      END SUBROUTINE WRONG4
