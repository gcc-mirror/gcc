! { dg-do compile }
! { dg-options "-c -Wall" }
!
! PR fortran/40041
! cf. also PR fortran/20373

subroutine valid_one
  REAL :: a
  INTEGER :: n
  INTRINSIC   ABS, MAX
  a(n) = MAX(ABS(2),ABS(3),n)
end subroutine

subroutine valid_two
  IMPLICIT NONE
  REAL :: a
  INTEGER :: n
  INTRINSIC   ABS, MAX
  a(n) = MAX(ABS(2),ABS(3),n)
end subroutine

subroutine warnings_one
  REAL :: a
  INTEGER :: n
  REAL :: ABS ! { dg-warning "Type specified for intrinsic function" }
  REAL :: MAX ! { dg-warning "Type specified for intrinsic function" }
  INTRINSIC   ABS, MAX
  a(n) = MAX(ABS(2),ABS(3),n)
end subroutine

subroutine warnings_two
  IMPLICIT NONE
  REAL :: a
  INTEGER :: n
  INTRINSIC ABS ! { dg-warning "Type specified for intrinsic function" }
  INTRINSIC MAX ! { dg-warning "Type specified for intrinsic function" }
  REAL :: ABS
  REAL :: MAX
  a(n) = MAX(ABS(2),ABS(3),n)
end subroutine
