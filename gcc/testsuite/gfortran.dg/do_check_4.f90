! { dg-do run }
! { dg-options "-fcheck=do" }
! { dg-shouldfail "DO check" }
!
! PR fortran/34656
! Run-time check for modifing loop variables
!
PROGRAM test
  IMPLICIT NONE
  INTEGER :: i
  DO i=1,100
    CALL do_something()
  ENDDO
CONTAINS
 SUBROUTINE do_something()
 IMPLICIT NONE
   DO i=1,10
   ENDDO
 END SUBROUTINE do_something
END PROGRAM test
! { dg-output "Fortran runtime error: Loop variable has been modified" }
