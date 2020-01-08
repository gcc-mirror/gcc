! { dg-do run }
! { dg-options "-fdec -Wconversion" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

include "dec-comparison-character_1.f90"

! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 8 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 9 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 10 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 11 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 12 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 13 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 14 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 15 }
! { dg-warning "HOLLERITH to CHARACTER" " " { target *-*-* } 16 }

