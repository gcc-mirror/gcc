! { dg-do compile }
! { dg-options "-fdec -fno-dec-char-conversions" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_data_1.f90"

! { dg-error "Incompatible types" " " { target *-*-* } 17 }
! { dg-error "Incompatible types" " " { target *-*-* } 18 }
! { dg-error "Incompatible types" " " { target *-*-* } 19 }
! { dg-error "Incompatible types" " " { target *-*-* } 20 }
! { dg-error "Incompatible types" " " { target *-*-* } 42 }
! { dg-error "Incompatible types" " " { target *-*-* } 43 }
! { dg-error "Incompatible types" " " { target *-*-* } 44 }
! { dg-error "Incompatible types" " " { target *-*-* } 45 }
! { dg-error "Incompatible types" " " { target *-*-* } 67 }
! { dg-error "Incompatible types" " " { target *-*-* } 68 }
! { dg-error "Incompatible types" " " { target *-*-* } 69 }
! { dg-error "Incompatible types" " " { target *-*-* } 70 }
