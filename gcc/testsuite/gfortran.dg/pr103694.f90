! { dg-do compile }
! PR fortran/103694 - ICE in gfc_conv_expr_op
! Contributed by G.Steinmetz

subroutine s
  type t
     integer :: a(2)
  end type
  type(t) :: x((0.)/0)
  integer :: n = size(x(1)%a) ! { dg-error "does not reduce to a constant expression" }
end
