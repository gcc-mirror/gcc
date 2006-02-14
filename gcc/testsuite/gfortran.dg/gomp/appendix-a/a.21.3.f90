! { dg-do compile }

      SUBROUTINE A21_GOOD(N)
      INTEGER N
!$OMP DO ORDERED
        DO I = 1,N
          IF (I <= 10) THEN
!$OMP ORDERED
              CALL WORK(I)
!$OMP END ORDERED
          ENDIF
          IF (I > 10) THEN
!$OMP ORDERED
              CALL WORK(I+1)
!$OMP END ORDERED
          ENDIF
        ENDDO
      END SUBROUTINE A21_GOOD
