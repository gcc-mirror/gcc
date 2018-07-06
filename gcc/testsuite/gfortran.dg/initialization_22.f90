! { dg-do run }
! tests the fix for PR39292, where the intitialization expression
! did not simplify and caused an ICE in gfc_conv_array_initializer.
!
! Contributed by Richard Guenther <rguenth@gcc.gnu.org>
!
  integer :: n
  real, dimension(2) :: a = (/ ( (float(n))**(1.0), n=1,2) /)
  if (any (a .ne. (/ ( (float(n))**(1.0), n=1,2) /))) STOP 1
end
