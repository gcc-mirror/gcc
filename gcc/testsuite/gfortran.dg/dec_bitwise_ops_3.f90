! { dg-do compile }
! { dg-options "-std=legacy -fdec -fno-dec" }
!
! PR fortran/87919
!
! Make sure -fno-dec disables bitwise ops and check for the right errors.
! -std=legacy is added to avoid the .XOR. extension warning.
!

include 'dec_bitwise_ops_1.f90'

! { dg-error "Operands of logical operator" " " { target *-*-* } 33 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 34 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 35 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 46 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 47 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 48 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 59 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 60 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 61 }
! { dg-error "Operand of .not. operator" " " { target *-*-* } 72 }
! { dg-error "Operand of .not. operator" " " { target *-*-* } 73 }
! { dg-error "Operand of .not. operator" " " { target *-*-* } 74 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 85 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 86 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 87 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 98 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 99 }
! { dg-error "Operands of logical operator" " " { target *-*-* } 100 }
