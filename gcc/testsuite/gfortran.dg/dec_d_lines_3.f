! { dg-do compile }
! { dg-options "-ffixed-form -fdec -fno-dec" }
!
! PR fortran/87919
!
! Ensure -fno-dec disables -fdec, leaving d-lines as code by default.
!

include 'dec_d_lines_2.f'

! { dg-error "character in statement label" " " { target *-*-* } 6 }
! { dg-error "character in statement label" " " { target *-*-* } 7 }
