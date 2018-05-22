! { dg-do compile }
! { dg-additional-options "-std=legacy" }

      SUBROUTINE WORK(I, J)
      INTEGER I,J
      END SUBROUTINE WORK
      SUBROUTINE A6_GOOD()
        INTEGER I, J
        REAL A(1000)
        DO 100 I = 1,10
!$OMP DO
           DO 100 J = 1,10
              CALL WORK(I,J)
            100     CONTINUE        ! !$OMP ENDDO implied here
!$OMP DO
        DO 200 J = 1,10
200        A(I) = I + 1
!$OMP ENDDO
!$OMP DO
        DO 300 I = 1,10
           DO 300 J = 1,10
              CALL WORK(I,J)
300     CONTINUE
!$OMP ENDDO
      END SUBROUTINE A6_GOOD
