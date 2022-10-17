! { dg-do compile }
!
! PR fortran/103789
! Check the absence of ICE when generating calls to VERIFY with a KIND argument.

program p
   character(len=10) :: y(2)
   integer :: z(2), x(2), w(2), v(2)
   y = ['abc', 'def']
   z = verify(y, 'e', kind=4) + 1
   x = verify(y, 'e', back=.false., kind=4) + 1
   w = verify(y, 'e',      .false., kind=4) + 1
   x = verify(y, 'e',      .false.,      4) + 1
end program p
