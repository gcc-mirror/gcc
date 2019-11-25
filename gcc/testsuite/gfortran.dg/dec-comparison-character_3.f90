! { dg-do compile }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

include "dec-comparison-character_1.f90"

! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 8 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 9 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 10 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 11 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 12 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 13 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 14 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 15 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 16 }
! { dg-warning "Extension: Conversion from HOLLERITH to CHARACTER" " " { target *-*-* } 8 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 9 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 10 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 11 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 12 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 13 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 14 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 15 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 16 }

