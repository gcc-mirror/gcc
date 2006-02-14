! { dg-do compile }
      SUBROUTINE WORK(I)
      INTEGER I
        I=I+1
      END SUBROUTINE WORK
      SUBROUTINE INCORRECT()
        INTEGER OMP_GET_NUM_THREADS
        INTEGER I, NP
        NP = OMP_GET_NUM_THREADS()   !misplaced: will return 1
!$OMP PARALLEL DO SCHEDULE(STATIC)
          DO I = 0, NP-1
            CALL WORK(I)
          ENDDO
!$OMP END PARALLEL DO
      END SUBROUTINE INCORRECT
