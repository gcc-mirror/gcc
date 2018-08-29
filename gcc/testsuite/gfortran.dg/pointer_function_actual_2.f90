! { dg-do run }
! Tests the fix for PR31200, in which the target x would
! not be associated with p
!
! COntributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  REAL,TARGET :: x
  CALL s3(f(x))
CONTAINS
  FUNCTION f(a)
    REAL,POINTER :: f
    REAL,TARGET :: a
    f => a
  END FUNCTION
  SUBROUTINE s3(targ)
    REAL,TARGET :: targ
    REAL,POINTER :: p
    p => targ
    IF (.NOT. ASSOCIATED(p,x)) STOP 1
  END SUBROUTINE
END

