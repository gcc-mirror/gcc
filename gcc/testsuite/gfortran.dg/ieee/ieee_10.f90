! { dg-do run }
! { dg-additional-options "-ffpe-trap=overflow,invalid" }
!
! Use dg-additional-options rather than dg-options to avoid overwriting the
! default IEEE options which are passed by ieee.exp and necessary.
program foo

   use ieee_arithmetic

   implicit none

   real x
   real(8) y

   ! At this point it is unclear what the behavior should be
   ! for -ffpe-trap=invalid with a signaling NaN
   !x = ieee_value(x, ieee_signaling_nan)
   !if (.not. ieee_is_nan(x)) stop 1
   x = ieee_value(x, ieee_quiet_nan)
   if (.not. ieee_is_nan(x)) stop 2

   x = ieee_value(x, ieee_positive_inf)
   if (ieee_is_finite(x)) stop 3
   x = ieee_value(x, ieee_negative_inf)
   if (ieee_is_finite(x)) stop 4

   ! At this point it is unclear what the behavior should be
   ! for -ffpe-trap=invalid with a signaling NaN
   !y = ieee_value(y, ieee_signaling_nan)
   !if (.not. ieee_is_nan(y)) stop 5
   y = ieee_value(y, ieee_quiet_nan)
   if (.not. ieee_is_nan(y)) stop 6

   y = ieee_value(y, ieee_positive_inf)
   if (ieee_is_finite(y)) stop 7
   y = ieee_value(y, ieee_negative_inf)
   if (ieee_is_finite(y)) stop 8

end program foo
