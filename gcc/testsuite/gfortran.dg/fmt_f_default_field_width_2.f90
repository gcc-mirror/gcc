! { dg-do run }
! { dg-options "-cpp -fdec-format-defaults" }
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
    implicit none
    character(50) :: buffer

    real(4) :: real_4
    real(8) :: real_8
#ifdef __GFC_REAL_16__
    real(16) :: real_16
#endif
    integer :: len
    character(*), parameter :: fmt = "(A, F, A)"

    real_4 = 4.18
    write(buffer, fmt) ':',real_4,':'
    print *,buffer
    if (buffer.ne.":      4.1799998:") stop 1

    real_4 = 0.00000018
    write(buffer, fmt) ':',real_4,':'
    print *,buffer
    if (buffer.ne.":      0.0000002:") stop 2

    real_8 = 4.18
    write(buffer, fmt) ':',real_8,':'
    print *,buffer
    len = len_trim(buffer)
    if (len /= 27) stop 3

#ifdef __GFC_REAL_16__
    real_16 = 4.18
    write(buffer, fmt) ':',real_16,':'
    print *,buffer
    len = len_trim(buffer)
    if (len /= 44) stop 4
#endif
end
