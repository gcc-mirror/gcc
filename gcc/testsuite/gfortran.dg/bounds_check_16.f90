! { dg-do compile }
! { dg-options "-fcheck=bounds" }
!
! PR fortran/50815
!
! Don't check the bounds of deferred-length strings.
! gfortran had an ICE before because it did.
!
SUBROUTINE TEST(VALUE)
    IMPLICIT NONE
    CHARACTER(LEN=:),    ALLOCATABLE    ::    VALUE
    CHARACTER(LEN=128)    ::    VAL
    VALUE = VAL
END SUBROUTINE TEST
