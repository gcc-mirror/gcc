! { dg-do compile }
!
! PR fortran/99798
! This example used to trigger an ICE caused by a premature release of the G
! symbol (with its argument X) following the rejection of the subroutine in
! the submodule.

module m
   interface
      module integer function g(x)
         integer, intent(in) :: x
      end
   end interface
end
submodule(m) m2
contains
   subroutine g(x)      ! { dg-error "FUNCTION attribute conflicts with SUBROUTINE" }
     integer, intent(in) :: x  ! { dg-error "Unexpected data declaration" }
   end
end
