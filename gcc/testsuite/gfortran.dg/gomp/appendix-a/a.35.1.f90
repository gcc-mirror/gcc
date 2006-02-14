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
!$OMP DO              ! incorrect nesting of loop regions
             DO J = 1, N
                CALL WORK(I,J)
             END DO
          END DO
!$OMP END PARALLEL
      END SUBROUTINE WRONG1
