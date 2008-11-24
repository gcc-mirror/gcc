! { dg-do compile }

! PR fortran/37779
! Check that using a non-recursive procedure as "value" is an error.

MODULE m
  IMPLICIT NONE

CONTAINS

  SUBROUTINE test ()
    IMPLICIT NONE
    PROCEDURE(test), POINTER :: procptr

    CALL bar (test) ! { dg-warning "Non-RECURSIVE" }
    procptr => test ! { dg-warning "Non-RECURSIVE" }
  END SUBROUTINE test

  INTEGER FUNCTION func ()
    ! Using a result variable is ok of course!
    func = 42 ! { dg-bogus "Non-RECURSIVE" }
  END FUNCTION func

END MODULE m

! { dg-final { cleanup-modules "m" } }
