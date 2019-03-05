! { dg-do compile }
! { dg-options "-fdec -fno-dec" }
!
! PR fortran/87919
!
! Ensure that -fno-dec disables the usage of TYPE as an alias for PRINT.
!

include 'dec_type_print.f90'

! { dg-error "Invalid character in name" "" { target *-*-* } 52 }
! { dg-error "Invalid character in name" "" { target *-*-* } 53 }
! { dg-error "Invalid character in name" "" { target *-*-* } 54 }
! { dg-error "Invalid character in name" "" { target *-*-* } 55 }
! { dg-error "Invalid character in name" "" { target *-*-* } 56 }
! { dg-error "Invalid character in name" "" { target *-*-* } 57 }
! { dg-error "Invalid character in name" "" { target *-*-* } 58 }
! { dg-error "conflicts with PROCEDURE" "" { target *-*-* } 60 }
! { dg-error "Cannot assign to a named constant" "" { target *-*-* } 80 }

