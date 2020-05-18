! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/78240
!
! Test a regression where an ICE occurred attempting to create array variables
! with non-constant array-specs in legacy clist initializers.
!
! Error message update with patch for PR fortran/83633
!
program p
  implicit none
  integer :: nn
  real :: rr
  structure /s/
    integer x(n)    /1/   ! { dg-error "must be constant of INTEGER type" }
    integer xx(nn)  /1/   ! { dg-error "array with nonconstant bounds" }
    integer xxx(rr) /1.0/ ! { dg-error "must be constant of INTEGER type" }
  end structure
end
