! { dg-do compile }
! { dg-options "-cpp -fdec -fno-dec-format-defaults" }
!
! Test case for the default field widths not enabled.
!
! Test case added by Mark Eggleston <mark.eggleston@codethink.com> to check
! use of -fno-dec-format-defaults
!

program test
    implicit none
    character(50) :: buffer

    real(4) :: real_4
    real(8) :: real_8
#ifdef __GFC_REAL_16__
    real(16) :: real_16
#endif
    integer :: len
    character(*), parameter :: fmt = "(A, G, A)"

    real_4 = 4.18
    write(buffer, fmt) ':',real_4,':' ! { dg-error "Positive width required" }

    real_4 = 0.00000018
    write(buffer, fmt) ':',real_4,':' ! { dg-error "Positive width required" }

    real_4 = 18000000.4
    write(buffer, fmt) ':',real_4,':' ! { dg-error "Positive width required" }

    real_8 = 4.18
    write(buffer, fmt) ':',real_8,':' ! { dg-error "Positive width required" }

#ifdef __GFC_REAL_16__
    real_16 = 4.18
    write(buffer, fmt) ':',real_16,':' ! { dg-error "Positive width required" "" { target fortran_real_16 } }
#endif
end
