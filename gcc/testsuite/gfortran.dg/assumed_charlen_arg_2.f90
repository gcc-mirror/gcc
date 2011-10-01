! { dg-do compile }
!
! PR 50585: [4.6/4.7 Regression] ICE with assumed length character array argument
!
! Contributed by Stuart Mentzer <sgm@objexx.com>

SUBROUTINE SUB1( str )
  IMPLICIT NONE
  CHARACTER(len=*) :: str(2)
  CALL SUB2( str(1)(:3) )
END SUBROUTINE

SUBROUTINE SUB2( str )
  IMPLICIT NONE
  CHARACTER(*) :: str
END SUBROUTINE 
