! { dg-do run }
! { dg-options "-fbounds-check" }
!
! Test the fix for PR52162 in which the elemental and conversion
! intrinsics in lines 14 and 19 would cause the bounds check to fail.
!
! Contributed by Dominique d'Humieres  <dominiq@lps.ens.fr>
!
    integer(4), allocatable :: a(:)
    integer(8), allocatable :: b(:)
    real, allocatable :: c(:)
    allocate (b(7:11), source = [7_8,8_8,9_8,10_8,11_8])

    a = b ! Implicit conversion

    if (lbound (a, 1) .ne. lbound(b, 1)) STOP 1
    if (ubound (a, 1) .ne. ubound(b, 1)) STOP 2

    c = sin(real(b(9:11))/100_8) ! Elemental intrinsic

    if ((ubound(c, 1) - lbound(c, 1)) .ne. 2) STOP 3
    if (any (nint(asin(c)*100.0) .ne. b(9:11))) STOP 4
    deallocate (a, b, c)
  end
