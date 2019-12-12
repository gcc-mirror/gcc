! { dg-do run }
! { dg-options "-fcheck-array-temporaries -fno-check-array-temporaries" }
!
! PR fortran/87919
!
! Ensure -fno-check-array-temporaries disables array temporary checking.
!

! Note that 'include' drops the dg-output check from the original test case.
include 'array_temporaries_2.f90'
