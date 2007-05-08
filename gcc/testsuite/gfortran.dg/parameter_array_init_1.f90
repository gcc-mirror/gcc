! { dg-do compile }
! tests the fix for PR29397, in which the initializer for the parameter
! 'J' was not expanded into an array.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
  INTEGER :: K(3) = 1
  INTEGER, PARAMETER :: J(3) = 2
  IF (ANY (MAXLOC (K, J<3) .NE. 1)) CALL ABORT ()
  IF (ANY (J .NE. 2)) CALL ABORT ()
END
