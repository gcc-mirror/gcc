! { dg-do compile }
!
! PR fortran/37821
!
! Ensure that for #include "..." and for include the
! current directory/directory of the source file is
! included. See also include_5.f90

subroutine one()
  include "include_4.inc"
  integer(i4) :: i
end subroutine one
