! { dg-do compile }
! Tests patch for PR29431, which arose from PR29373.
!
! Contributed by Tobias Schlueter  <tobi@gcc.gnu.org>
!
  implicit none
  CHARACTER(len=6), DIMENSION(2,2)  :: a

! Reporters original triggered another error:
! gfc_todo: Not Implemented: complex character array
! constructors.

  a = reshape([to_string(1.0), trim("abcdef"), &
               to_string(7.0), trim("hijklm")], &
               [2, 2])
  print *, a

  CONTAINS
    FUNCTION to_string(x)
      character*6 to_string
      REAL, INTENT(in) :: x
      WRITE(to_string, FMT="(F6.3)") x
    END FUNCTION
end
