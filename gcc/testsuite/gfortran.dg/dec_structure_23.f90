! { dg-do compile }
! { dg-options "-fdec-structure" }
!
! PR fortran/78240
!
! Test a regression where an ICE occurred attempting to create array variables
! with non-constant array-specs in legacy clist initializers.
!

program p
  implicit none
  integer :: nn
  real :: rr
  structure /s/
    integer x(n)    /1/   ! { dg-error "xpected constant" }
    integer xx(nn)  /1/   ! { dg-error "xpected constant" }
    integer xxx(rr) /1.0/ ! { dg-error "xpected constant" }
  end structure
end
