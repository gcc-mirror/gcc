! { dg-do compile }
! { dg-options "-fdec -fno-dec-char-conversions" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_assignment_1.f90"

! { dg-error "Cannot convert" " " { target *-*-* } 16 }
! { dg-error "Cannot convert" " " { target *-*-* } 17 }
! { dg-error "Cannot convert" " " { target *-*-* } 18 }
! { dg-error "Cannot convert" " " { target *-*-* } 19 }
! { dg-error "Cannot convert" " " { target *-*-* } 31 }
! { dg-error "Cannot convert" " " { target *-*-* } 32 }
! { dg-error "Cannot convert" " " { target *-*-* } 33 }
! { dg-error "Cannot convert" " " { target *-*-* } 34 }
! { dg-error "Cannot convert" " " { target *-*-* } 46 }
! { dg-error "Cannot convert" " " { target *-*-* } 47 }
! { dg-error "Cannot convert" " " { target *-*-* } 48 }
! { dg-error "Cannot convert" " " { target *-*-* } 49 }
