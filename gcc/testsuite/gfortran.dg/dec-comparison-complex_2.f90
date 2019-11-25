! { dg-do run }
! { dg-options "-fdec -Wconversion" }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>
!

include "dec-comparison-complex_1.f90"

! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 10 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 13 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 14 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 15 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 16 }

