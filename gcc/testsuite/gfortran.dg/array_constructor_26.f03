! { dg-do compile }
! { dg-options "-std=gnu" }

! PR fortran/36492
! Check for incorrect error message with -std=f2003.
! Test from comment #4, PR 36492 causing ICE.

MODULE WinData
  IMPLICIT NONE
  INTEGER (1), PARAMETER :: MAXFLD = 25_1, MAXHED = 5_1, MAXCHR = 80_1
  integer :: i
  TYPE TWindowData
    CHARACTER (MAX_FLD_HED, 1) :: DWFdHd(MAXFLD) = [(" ", i = 1, MAXFLD)]
    ! { dg-error "no IMPLICIT type" "" { target *-*-* } 13 }
    ! { dg-error "specification expression" "" { target *-*-* } 13 }
  END TYPE TWindowData
END MODULE WinData
