! { dg-do run }
! { dg-options "-fdec -Wcharacter-truncation" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_assignment_1.f90"

! { dg-warning "is truncated in conversion" " " { target *-*-* } 46 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 47 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 48 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 49 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 50 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 51 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 52 }
! { dg-warning "is truncated in conversion" " " { target *-*-* } 53 }

