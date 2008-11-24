! { dg-do compile }
! { dg-options "-frecursive" }

! PR fortran/37779
! Check that -frecursive allows using procedures in as procedure expressions.

MODULE m
  IMPLICIT NONE

CONTAINS

  SUBROUTINE test ()
    IMPLICIT NONE
    PROCEDURE(test), POINTER :: procptr

    CALL bar (test) ! { dg-bogus "Non-RECURSIVE" }
    procptr => test ! { dg-bogus "Non-RECURSIVE" }
  END SUBROUTINE test

  INTEGER FUNCTION func ()
    ! Using a result variable is ok of course!
    func = 42 ! { dg-bogus "Non-RECURSIVE" }
  END FUNCTION func

END MODULE m

! { dg-final { cleanup-modules "m" } }
