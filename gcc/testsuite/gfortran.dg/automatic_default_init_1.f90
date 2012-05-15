! { dg-do run }
! { dg-options "-O" }
! Test the fix for PR29394 in which automatic arrays did not
! get default initialization.
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org> 
!
MODULE M1
  TYPE T1 
    INTEGER :: I=7 
  END TYPE T1 
CONTAINS 
  SUBROUTINE S1(I) 
    INTEGER, INTENT(IN) :: I 
    TYPE(T1) :: D(1:I)
    IF (any (D(:)%I.NE.7)) CALL ABORT() 
  END SUBROUTINE S1 
END MODULE M1
  USE M1 
  CALL S1(2) 
END 
