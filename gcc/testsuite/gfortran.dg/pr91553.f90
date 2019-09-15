! { dg-do run }
! Code contributed by Gerhard Steinmetz
program p
   complex z(1)
   z = (1.0, 2.0) * [real :: (3.0 + 4.0)]
   if (real(z(1)) /= 7.) stop 1
   if (aimag(z(1)) /= 14.) stop 2
end
