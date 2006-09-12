! { dg-do compile }
! This caused an ICE for gfortrans of July 2006 vintage.  It was a regression
! that "fixed" itself.  The cause and the fix are mysteries.  This test is intended
! to signal any further regression, should it occur.
!
! Contributed by Oskar Enoksson  <enok@lysator.liu.se>

SUBROUTINE BUG(A,B)
  IMPLICIT NONE
  
  INTEGER   :: A
  INTEGER   :: B(2)
  
  INTEGER, PARAMETER :: C(2) = (/ 1,2 /)
  
  WHERE (C(:).EQ.A)
    B = -1
  END WHERE
END SUBROUTINE BUG

