! { dg-do compile }
!
! PR 48291: [4.6/4.7 Regression] [OOP] internal compiler error, new_symbol(): Symbol name too long
!
! Contributed by Adrian Prantl <adrian@llnl.gov>

module Overload_AnException_Impl
  type :: Overload_AnException_impl_t
  end type
contains
  subroutine ctor_impl(self)
    class(Overload_AnException_impl_t) :: self
  end subroutine
end module 
