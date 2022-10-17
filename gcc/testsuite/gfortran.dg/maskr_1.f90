! { dg-do compile }
!
! PR fortran/103789
! Check the absence of ICE when generating calls to MASKR with a KIND argument.

program p
   integer :: z(2), y(2), x(2)
   y = [1, 13]
   z = maskr(y, kind=4) + 1
   x = maskr(y,      4) + 1
end program p
