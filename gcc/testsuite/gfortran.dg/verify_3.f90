! { dg-do compile }
!
! PR fortran/103789
! Check the absence of ICE when generating calls to VERIFY with a KIND argument.

program p
   character(len=10) :: y(2)
   integer :: z(2)
   y = ['abc', 'def']
   z = verify(y, 'e', kind=4) + 1
end program p
