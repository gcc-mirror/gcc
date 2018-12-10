! { dg-do compile }
! { dg-options "-fdec -fno-dec" }
!
! PR fortran/87919
!
! Make sure -fno-dec rejects -fdec I/O specifiers as with dec_io_1.
!

include 'dec_io_1.f90'

! { dg-error "is a DEC extension" "" { target *-*-* } 12 }
! { dg-error "is a DEC extension" "" { target *-*-* } 24 }
! { dg-error "is a DEC extension" "" { target *-*-* } 58 }
! { dg-error "is a DEC extension" "" { target *-*-* } 64 }
! { dg-error "is a DEC extension" "" { target *-*-* } 68 }
! { dg-error "is a DEC extension" "" { target *-*-* } 74 }
! { dg-error "is a DEC extension" "" { target *-*-* } 78 }
! { dg-error "is a DEC extension" "" { target *-*-* } 84 }
! { dg-error "is a DEC extension" "" { target *-*-* } 90 }
! { dg-error "is a DEC extension" "" { target *-*-* } 96 }
