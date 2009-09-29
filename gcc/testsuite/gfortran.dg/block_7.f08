! { dg-do run }
! { dg-options "-std=f2008 -fall-intrinsics" }

! Check for correct placement (on the stack) of local variables with BLOCK
! and recursive container procedures.

RECURSIVE SUBROUTINE myproc (i)
  INTEGER, INTENT(IN) :: i
  ! Wrap the block up in some other construct so we see this doesn't mess
  ! things up, either.
  DO
    BLOCK
      INTEGER :: x
      x = i
      IF (i > 0) CALL myproc (i - 1)
      IF (x /= i) CALL abort ()
    END BLOCK
    EXIT
  END DO
END SUBROUTINE myproc

PROGRAM main
  CALL myproc (42)
END PROGRAM main
