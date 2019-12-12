! { dg-do run "xfail *-*-*" }
! { dg-options "-fdec -fno-dec" }
!
! PR fortran/87919
!
! Make sure -fno-dec disables -fdec as with dec_exp_3.
!

include 'dec_exp_3.f90'

! { XFAIL "Bad real number" "" { target *-*-* } 13 }
