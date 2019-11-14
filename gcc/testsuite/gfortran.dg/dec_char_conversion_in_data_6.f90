! { dg-do run }
! { dg-options "-fdec -Wconversion" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_data_1.f90"

! { dg-warning "Nonstandard conversion from CHARACTER\\(4\\)" " " { target *-*-* } 17 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(4\\)" " " { target *-*-* } 18 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(8\\)" " " { target *-*-* } 19 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(4\\)" " " { target *-*-* } 20 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 21 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 22 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 23 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 24 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(2\\)" " " { target *-*-* } 42 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(2\\)" " " { target *-*-* } 43 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(5\\)" " " { target *-*-* } 44 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(3\\)" " " { target *-*-* } 45 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 46 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 47 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 48 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 49 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(6\\)" " " { target *-*-* } 67 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(6\\)" " " { target *-*-* } 68 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(10\\)" " " { target *-*-* } 69 }
! { dg-warning "Nonstandard conversion from CHARACTER\\(5\\)" " " { target *-*-* } 70 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 71 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 72 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 73 }
! { dg-warning "Conversion from HOLLERITH" " " { target *-*-* } 74 }


