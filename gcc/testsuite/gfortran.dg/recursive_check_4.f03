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

  INTEGER FUNCTION test2 () RESULT (x)
    IMPLICIT NONE
    PROCEDURE(test2), POINTER :: procptr

    CALL bar (test2) ! { dg-warning "Non-RECURSIVE" }
    procptr => test2 ! { dg-warning "Non-RECURSIVE" }

    x = 1812
  END FUNCTION test2

  INTEGER FUNCTION func ()
    ! Using a result variable is ok of course!
    func = 42 ! { dg-bogus "Non-RECURSIVE" }
  END FUNCTION func

END MODULE m
