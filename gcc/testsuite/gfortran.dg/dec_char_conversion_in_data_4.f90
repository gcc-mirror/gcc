! { dg-do run }
! { dg-options "-fdec -Wcharacter-truncation" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_data_1.f90"

! { dg-warning "character constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 67 }
! { dg-warning "character constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 68 }
! { dg-warning "character constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 69 }
! { dg-warning "character constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 70 }
! { dg-warning "Hollerith constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 71 }
! { dg-warning "Hollerith constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 72 }
! { dg-warning "Hollerith constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 73 }
! { dg-warning "Hollerith constant at \\(1\\) is truncated in conversion" " " { target *-*-* } 74 }


