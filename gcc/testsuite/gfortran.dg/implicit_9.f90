! { dg-do compile }
! Tests patch for PR29373, in which the implicit character
! statement messes up the function declaration because the
! requisite functions in decl.c were told nothing about
! implicit types.
!
! Contributed by Tobias Schlueter  <tobi@gcc.gnu.org>
!
  implicit character*32 (a-z)
  CHARACTER(len=255), DIMENSION(1,2)  :: a

! Reporters original, which triggers another error:
! gfc_todo: Not Implemented: complex character array
! constructors.=> PR29431
!  a = reshape((/ to_string(1.0) /), (/ 1, 2 /))

  a = to_string(1.0)
  print *, a
  CONTAINS
    CHARACTER*(32) FUNCTION to_string(x)
      REAL, INTENT(in) :: x
      WRITE(to_string, FMT="(F6.3)") x
    END FUNCTION
END PROGRAM