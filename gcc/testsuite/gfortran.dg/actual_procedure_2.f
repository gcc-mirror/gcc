! { dg-do compile }
! PR fortran/50377
!
! Reject procedures passed as actual argument if there is no explicit
! interface and they are not declared EXTERNAL
!
! Contributed by Vittorio Zecca

!     external sub      ! Required for valid code
!     external fun      ! Required for valid code
      call sub(sub)     ! { dg-error "used as actual argument" }
      z = fun(fun)      ! { dg-error "used as actual argument" }
      end

      subroutine sub(y)
      external y
      end

      real function fun(z)
      external z
      f = 1.
      end
