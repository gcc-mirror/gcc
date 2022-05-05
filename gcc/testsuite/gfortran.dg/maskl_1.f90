! { dg-do compile }
!
! PR fortran/103789
! Check the absence of ICE when generating calls to MASKL with a KIND argument.

program p
   integer :: z(2), y(2), x(2)
   y = [1, 13]
   z = maskl(y, kind=4) + 1
   x = maskl(y,      4) + 1
end program p
