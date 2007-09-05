! { dg-do compile }
! Checks the fix for PR33241, in which the assumed character
! length of the parameter was never filled in with that of
! the initializer.
!
! Contributed by Victor Prosolin <victor.prosolin@gmail.com>
!
PROGRAM fptest
  IMPLICIT NONE
  CHARACTER (LEN=*), DIMENSION(1),  PARAMETER :: var  = 'a'
  CALL parsef (var)
contains
  SUBROUTINE parsef (Var)
    IMPLICIT NONE
    CHARACTER (LEN=*), DIMENSION(:), INTENT(in) :: Var
  END SUBROUTINE parsef
END PROGRAM fptest
