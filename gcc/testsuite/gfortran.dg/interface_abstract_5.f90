! { dg-do compile }
!
! PR 71861: [7/8/9 Regression] [F03] ICE in write_symbol(): bad module symbol
!
! Contributed by Gerhard Steinmetz <gerhard.steinmetz.fortran@t-online.de>

module m1
   intrinsic abs
   abstract interface
      function abs(x)    ! { dg-error "ABSTRACT attribute conflicts with INTRINSIC attribute" }
         real :: abs, x
      end
   end interface
end

module m2
   abstract interface
      function abs(x)
         real :: abs, x
      end
   end interface
   intrinsic abs    ! { dg-error "ABSTRACT attribute conflicts with INTRINSIC attribute" }
end

module m3
   abstract interface
      function f(x)
         real :: f, x
      end
   end interface
   intrinsic f    ! { dg-error "ABSTRACT attribute conflicts with INTRINSIC attribute" }
end
