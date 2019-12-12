! { dg-do compile }
subroutine convrs(quanty,fromto)
   implicit none

   character(*), intent(in) :: quanty,fromto

   if (len(fromto) /= 2) stop 1
   if (fromto /= 'OK') stop 2
end subroutine
