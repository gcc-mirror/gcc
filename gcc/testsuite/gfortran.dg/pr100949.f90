! { dg-do compile }
! PR fortran/100949 - ICE in gfc_conv_expr_present, at fortran/trans-expr.c:1975

subroutine s
entry f
  type t
  end type
  class(t), allocatable :: y, z
  allocate (z, mold=y)
end
