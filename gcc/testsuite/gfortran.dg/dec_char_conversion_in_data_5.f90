! { dg-do run }
! { dg-options "-fdec -Wsurprising" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_data_1.f90"

! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 20 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 24 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 45 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 49 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 70 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 74 }


