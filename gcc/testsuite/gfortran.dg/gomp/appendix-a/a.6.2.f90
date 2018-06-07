! { dg-do compile }
! { dg-additional-options "-std=legacy" }

        SUBROUTINE WORK(I, J)
        INTEGER I,J
        END SUBROUTINE WORK

        SUBROUTINE A6_WRONG
        INTEGER I, J
        DO 100 I = 1,10
!$OMP DO
        DO 100 J = 1,10
        CALL WORK(I,J)
        100      CONTINUE
!$OMP ENDDO	! { dg-error "Unexpected ..OMP END DO statement" }
        END SUBROUTINE A6_WRONG
