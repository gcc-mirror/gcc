! { dg-do compile }
! PR fortran/97245 - ASSOCIATED intrinsic did not recognize a
!                    pointer variable the second time it is used

MODULE formulaciones
  IMPLICIT NONE

  ABSTRACT INTERFACE
     SUBROUTINE proc_void()
     END SUBROUTINE proc_void
  end INTERFACE

  PROCEDURE(proc_void), POINTER :: pADJSensib => NULL()

CONTAINS

  subroutine calculo()
    PROCEDURE(proc_void), POINTER :: otherprocptr => NULL()

    IF (associated(pADJSensib)) THEN
       CALL pADJSensib ()
    ENDIF
    IF (associated(pADJSensib)) THEN    ! this was erroneously rejected
       CALL pADJSensib ()
    END IF

    IF (associated(otherprocptr)) THEN
       CALL otherprocptr ()
    ENDIF
    IF (associated(otherprocptr)) THEN
       CALL otherprocptr ()
    END IF
  end subroutine calculo

END MODULE formulaciones
