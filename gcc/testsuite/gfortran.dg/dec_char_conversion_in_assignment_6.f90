! { dg-do run }
! { dg-options "-fdec -Wsurprising" }
!
! Modified by Mark Eggleston <mark.eggleston@codethink.com>
!
include "dec_char_conversion_in_assignment_1.f90"

! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 19 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 23 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 34 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 38 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 49 }
! { dg-warning "Assigning value other than 0 or 1 to LOGICAL" " " { target *-*-* } 53 }

