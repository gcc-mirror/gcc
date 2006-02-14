! { dg-do compile }

      SUBROUTINE WORK(I)
      INTEGER I
      END SUBROUTINE WORK
      SUBROUTINE A21_WRONG(N)
      INTEGER N
        INTEGER I
!$OMP DO ORDERED
        DO I = 1, N
! incorrect because an iteration may not execute more than one
! ordered region
!$OMP ORDERED
            CALL WORK(I)
!$OMP END ORDERED
!$OMP ORDERED
            CALL WORK(I+1)
!$OMP END ORDERED
        END DO
      END SUBROUTINE A21_WRONG
