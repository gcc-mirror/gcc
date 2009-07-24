! { dg-do compile }
!
! PR 40822: [4.5 Regression] Internal compiler error when Fortran intrinsic LEN referenced before explicit declaration
!
! Contributed by Mat Cross <mathewc@nag.co.uk>

SUBROUTINE SEARCH(ITEMVAL)
  CHARACTER (*) :: ITEMVAL
  CHARACTER (LEN(ITEMVAL)) :: ITEM
  INTRINSIC LEN
END

