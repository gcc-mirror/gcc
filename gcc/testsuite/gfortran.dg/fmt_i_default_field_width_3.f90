! { dg-do compile }
! { dg-options "-fdec -fno-dec-format-defaults" }
!
! Test case for the default field widths enabled by the -fdec-format-defaults flag.
!
! This feature is not part of any Fortran standard, but it is supported by the
! Oracle Fortran compiler and others.
!
! Test case added by Mark Eggleston <mark.eggleston@codethink.com> to check
! use of -fdec-format-defaults
!

program test
    character(50) :: buffer
    character(1) :: colon

    integer(2) :: integer_2
    integer(4) :: integer_4
    integer(8) :: integer_8
    character(*), parameter :: fmt = "(A, I, A)"

    write(buffer, fmt) ':',12340,':' ! { dg-error "Nonnegative width required" }

    read(buffer, '(A1, I, A1)') colon, integer_4, colon ! { dg-error "Nonnegative width required" }
    if (integer_4.ne.12340) stop 2

    integer_2 = -99
    write(buffer, fmt) ':',integer_2,':' ! { dg-error "Nonnegative width required" }

    integer_8 = -11112222
    write(buffer, fmt) ':',integer_8,':' ! { dg-error "Nonnegative width required" }

! If the width is 7 and there are 7 leading zeroes, the result should be zero.
    integer_2 = 789
    buffer = '0000000789'
    read(buffer, '(I)') integer_2 ! { dg-error "Nonnegative width required" }
end
