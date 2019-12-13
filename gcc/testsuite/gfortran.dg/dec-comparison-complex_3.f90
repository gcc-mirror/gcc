! { dg-do compile }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

include "dec-comparison-complex_1.f90"

! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 10 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 13 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 14 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 15 }
! { dg-warning "Legacy Extension: Hollerith constant at" " " { target *-*-* } 16 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 10 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 13 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 14 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 15 }
! { dg-error "Operands of comparison operator" " " { target *-*-* } 16 }

