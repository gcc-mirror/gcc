! { dg-do compile }
! PR 49802
! Only a scalar VALUE dummy is interoperable with a formal parameter of
! the C prototype (F2023, 18.3.6 (4)), so an array VALUE dummy is not
! allowed in a BIND(C) procedure.  A polymorphic array VALUE dummy is
! not yet implemented; it used to ICE.

module m
  use iso_c_binding
  implicit none
  type :: t
    integer :: i = 0
  end type
contains

  subroutine bindc_expl (x) bind(c) ! { dg-error "not allowed in BIND\\(C\\) procedure" }
    integer(c_int), value :: x(3)
  end subroutine

  subroutine bindc_ashape (x) bind(c) ! { dg-error "not allowed in BIND\\(C\\) procedure" }
    integer(c_int), value :: x(:)
  end subroutine

  subroutine poly (x) ! { dg-error "not yet implemented" }
    class(t), value :: x(:)
  end subroutine

end module
