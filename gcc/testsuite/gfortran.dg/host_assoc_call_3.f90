! { dg-do compile }
!
! PR fortran/37445, in which the contained 'putaline' would be
! ignored and no specific interface found in the generic version.
!
! Contributed by Norman S Clerman < clerman@fuse.net>
!
MODULE M1
  INTERFACE putaline
    MODULE PROCEDURE S1,S2
  END INTERFACE
CONTAINS
  SUBROUTINE S1(I)
  END SUBROUTINE
  SUBROUTINE S2(F)
  END SUBROUTINE
END MODULE

MODULE M2
  USE M1
CONTAINS
  SUBROUTINE S3
    integer :: check = 0
    CALL putaline()
    if (check .ne. 1) call abort
    CALL putaline("xx")
    if (check .ne. 2) call abort
!  CALL putaline(1.0) ! => this now causes an error, as it should 
  CONTAINS
    SUBROUTINE putaline(x)
      character, optional :: x
      if (present(x)) then
        check = 2
      else
        check = 1
      end if
    END SUBROUTINE
  END SUBROUTINE
END MODULE

  USE M2
  CALL S3
END
! { dg-final { cleanup-modules "M1 M2" } }
