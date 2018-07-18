! { dg-do compile }
! { dg-additional-options "-fopenmp" }
      SUBROUTINE ICHI
      INTEGER :: ARGC
      ARGC = COMMAND_ARGUMENT_COUNT ()

!$OMP PARALLEL
!$ACC PARALLEL                                                          &
!$ACC& COPYIN(ARGC) ! { dg-error "directive cannot be specified within" }
      IF (ARGC .NE. 0) THEN
         STOP 1
      END IF
!$ACC END PARALLEL
!$OMP END PARALLEL

      END SUBROUTINE ICHI


      SUBROUTINE NI
      IMPLICIT NONE
      INTEGER :: I

!$ACC PARALLEL                                                          &
!$OMP& DO ! { dg-error "Wrong OpenACC continuation" }
      DO I = 1, 10
      ENDDO

!$OMP PARALLEL                                                          &
!$ACC& LOOP ! { dg-error "Wrong OpenMP continuation" }
      DO I = 1, 10
      ENDDO
      END SUBROUTINE NI
