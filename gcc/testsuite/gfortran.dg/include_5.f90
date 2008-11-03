! { dg-do compile }
! { dg-options "-cpp" }
!
! PR fortran/37821
!
! Ensure that for #include "..." and for include the
! current directory/directory of the source file is
! included.

subroutine one()
  include "include_4.inc"
  integer(i4) :: i
end subroutine one

subroutine two()
# include "include_4.inc"
  integer(i4) :: i
end subroutine two
