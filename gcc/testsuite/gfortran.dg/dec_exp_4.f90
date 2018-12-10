! { dg-do compile }
! { dg-options "-fdec -fno-dec" }
!
! PR fortran/87919
!
! Make sure -fno-dec disables -fdec as with dec_exp_2.
!

include 'dec_exp_2.f90'

! { dg-error "Missing exponent" "" { target *-*-* } 9 }
! { dg-error "Missing exponent" "" { target *-*-* } 11 }
