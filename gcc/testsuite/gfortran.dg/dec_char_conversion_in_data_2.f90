! { dg-do run }
! { dg-options "-fdec-char-conversions" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_data_1.f90"

! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 21 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 22 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 23 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 24 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 46 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 47 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 48 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 49 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 71 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 72 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 73 }
! { dg-warning "Legacy Extension: Hollerith constant" " " { target *-*-* } 74 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 21 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 22 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 23 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 24 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 46 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 47 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 48 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 49 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 71 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 72 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 73 }
! { dg-warning "Extension: Conversion from HOLLERITH" " " { target *-*-* } 74 }
! { dg-warning "Extension: Conversion from CHARACTER\\(4\\)" " " { target *-*-* } 17 }
! { dg-warning "Extension: Conversion from CHARACTER\\(4\\)" " " { target *-*-* } 18 }
! { dg-warning "Extension: Conversion from CHARACTER\\(8\\)" " " { target *-*-* } 19 }
! { dg-warning "Extension: Conversion from CHARACTER\\(4\\)" " " { target *-*-* } 20 }
! { dg-warning "Extension: Conversion from CHARACTER\\(2\\)" " " { target *-*-* } 42 }
! { dg-warning "Extension: Conversion from CHARACTER\\(2\\)" " " { target *-*-* } 43 }
! { dg-warning "Extension: Conversion from CHARACTER\\(5\\)" " " { target *-*-* } 44 }
! { dg-warning "Extension: Conversion from CHARACTER\\(3\\)" " " { target *-*-* } 45 }
! { dg-warning "Extension: Conversion from CHARACTER\\(6\\)" " " { target *-*-* } 67 }
! { dg-warning "Extension: Conversion from CHARACTER\\(6\\)" " " { target *-*-* } 68 }
! { dg-warning "Extension: Conversion from CHARACTER\\(10\\)" " " { target *-*-* } 69 }
! { dg-warning "Extension: Conversion from CHARACTER\\(5\\)" " " { target *-*-* } 70 }


