! { dg-do run }
!
! PR fortran/37445, in which the contained 's1' would be
! ignored and the use+host associated version used.
!
! Contributed by Norman S Clerman < clerman@fuse.net>
!
MODULE M1
CONTAINS
  integer function S1 ()
    s1 = 0
  END function
END MODULE

MODULE M2
  USE M1
CONTAINS
  SUBROUTINE S2
    if (s1 () .ne. 1) STOP 1
  CONTAINS
    integer function S1 ()
      s1 = 1
    END function
  END SUBROUTINE
END MODULE

  USE M2
  CALL S2
END
