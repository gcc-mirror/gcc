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
      i = 3
  END SUBROUTINE
  SUBROUTINE S2(F)
      f = 4.0
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
  subroutine S4
    integer :: check = 0
    REAL :: rcheck = 0.0
    call putaline(check)
    if (check .ne. 3) call abort
    call putaline(rcheck)
    if (rcheck .ne. 4.0) call abort
  end subroutine s4
END MODULE

  USE M2
  CALL S3
  call S4
END
! { dg-final { cleanup-modules "m1 m2" } }
