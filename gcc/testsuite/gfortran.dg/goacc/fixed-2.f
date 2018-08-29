! { dg-do compile } 
! { dg-additional-options "-fmax-errors=100" } 

      INTEGER :: ARGC
      ARGC = COMMAND_ARGUMENT_COUNT ()

!$OMP xPARALLEL
!$ACC xPARALLEL COPYIN(ARGC)	! { dg-error "Unclassifiable OpenACC directive" }
      IF (ARGC .NE. 0) THEN
         STOP 1
      END IF
!$ACC END PARALLEL 	! { dg-error "Unexpected" }
!$OMP END PARALLEL

      END
