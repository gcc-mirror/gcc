! { dg-do run }
program foo

   use ieee_arithmetic

   implicit none

   call test04
   call test08
   call test10
   call test16

   contains

      subroutine test04
         real(4) x, y 
         if (ieee_support_subnormal(x)) then
            x = ieee_value(x, ieee_positive_subnormal)
            y = ieee_value(y, ieee_positive_denormal)
            if (x /= y) stop 1
            x = ieee_value(x, ieee_negative_subnormal)
            y = ieee_value(y, ieee_negative_denormal)
            if (x /= y) stop 2
         end if
      end subroutine test04

      subroutine test08
         real(8) x, y 
         if (ieee_support_subnormal(x)) then
            x = ieee_value(x, ieee_positive_subnormal)
            y = ieee_value(y, ieee_positive_denormal)
            if (x /= y) stop 1
            x = ieee_value(x, ieee_negative_subnormal)
            y = ieee_value(y, ieee_negative_denormal)
            if (x /= y) stop 2
         end if
      end subroutine test08

#ifdef __GFC_REAL_10__
      subroutine test10
         real(10) x, y 
         if (ieee_support_subnormal(x)) then
            x = ieee_value(x, ieee_positive_subnormal)
            y = ieee_value(y, ieee_positive_denormal)
            if (x /= y) stop 1
            x = ieee_value(x, ieee_negative_subnormal)
            y = ieee_value(y, ieee_negative_denormal)
            if (x /= y) stop 2
         end if
      end subroutine test10
#else
      subroutine test10
      end subroutine test10
#endif

#ifdef __GFC_REAL_16__
      subroutine test16
         real(16) x, y 
         if (ieee_support_subnormal(x)) then
            x = ieee_value(x, ieee_positive_subnormal)
            y = ieee_value(y, ieee_positive_denormal)
            if (x /= y) stop 1
            x = ieee_value(x, ieee_negative_subnormal)
            y = ieee_value(y, ieee_negative_denormal)
            if (x /= y) stop 2
         end if
      end subroutine test16
#else
      subroutine test16
      end subroutine test16
#endif


end program foo
