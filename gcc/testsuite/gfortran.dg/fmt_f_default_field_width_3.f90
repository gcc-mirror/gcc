! { dg-do compile }
! { dg-options "-fdec -fno-dec-format-defaults" }
!
! Test case for the default field widths not enabled.
!
! Test case added by Mark Eggleston <mark.eggleston@codethink.com> to check
! use of -fno-dec-format-defaults
!

program test
    character(50) :: buffer

    real*4 :: real_4
    real*8 :: real_8
    real*16 :: real_16
    integer :: len
    character(*), parameter :: fmt = "(A, F, A)"

    real_4 = 4.18
    write(buffer, fmt) ':',real_4,':' ! { dg-error "Nonnegative width required" }

    real_4 = 0.00000018
    write(buffer, fmt) ':',real_4,':' ! { dg-error "Nonnegative width required" }

    real_8 = 4.18
    write(buffer, fmt) ':',real_8,':' ! { dg-error "Nonnegative width required" }

    real_16 = 4.18
    write(buffer, fmt) ':',real_16,':' ! { dg-error "Nonnegative width required" }
end
