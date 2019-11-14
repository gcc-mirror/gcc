! { dg-do run }
! { dg-options "-fdec-char-conversions" }
!
! Contributeds by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_assignment_1.f90"

! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 16 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 17 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 18 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 19 }
! { dg-warning "Extension: Hollerith constant" " " { target *-*-* } 20 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 21 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 22 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 23 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 20 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 21 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 22 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 23 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 31 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 32 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 33 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 34 }
! { dg-warning "Extension: Hollerith constant" " " { target *-*-* } 35 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 36 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 37 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 38 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 35 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 36 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 37 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 38 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 46 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 47 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 48 }
! { dg-warning "Extension: Conversion from CHARACTER" " " { target *-*-* } 49 }
! { dg-warning "Extension: Hollerith constant" " " { target *-*-* } 50 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 51 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 52 }
! { dg-warning "Extension: Hollerith constant"  " " { target *-*-* } 53 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 50 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 51 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 52 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 53 }

