! { dg-do run }
! { dg-options "-ffpe-trap=overflow,invalid" }
program foo

   use ieee_arithmetic

   implicit none

   real x
   real(8) y

   x = ieee_value(x, ieee_signaling_nan)
   if (.not. ieee_is_nan(x)) stop 1
   x = ieee_value(x, ieee_quiet_nan)
   if (.not. ieee_is_nan(x)) stop 2

   x = ieee_value(x, ieee_positive_inf)
   if (ieee_is_finite(x)) stop 3
   x = ieee_value(x, ieee_negative_inf)
   if (ieee_is_finite(x)) stop 4

   y = ieee_value(y, ieee_signaling_nan)
   if (.not. ieee_is_nan(y)) stop 5
   y = ieee_value(y, ieee_quiet_nan)
   if (.not. ieee_is_nan(y)) stop 6

   y = ieee_value(y, ieee_positive_inf)
   if (ieee_is_finite(y)) stop 7
   y = ieee_value(y, ieee_negative_inf)
   if (ieee_is_finite(y)) stop 8

end program foo
