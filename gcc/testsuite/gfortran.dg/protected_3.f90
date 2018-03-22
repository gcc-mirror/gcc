! { dg-options "-std=f95" }
! PR fortran/23994
!
! Test PROTECTED attribute. Within the module everything is allowed.
! Outside (use-associated): For pointers, their association status
! may not be changed. For nonpointers, their value may not be changed.
!
! Reject in Fortran 95

module protmod
  implicit none
  integer          :: a
  integer, target  :: at
  integer, pointer :: ap
  protected :: a, at, ap ! { dg-error "Fortran 2003: PROTECTED statement" }
end module protmod

module protmod2
  implicit none
  integer, protected          :: a  ! { dg-error "Fortran 2003: PROTECTED attribute" }
  integer, protected, target  :: at ! { dg-error "Fortran 2003: PROTECTED attribute" }
  integer, protected, pointer :: ap ! { dg-error "Fortran 2003: PROTECTED attribute" }
end module protmod2
