! { dg-do compile }
! { dg-options "-O2 -Wall" }
! PR fortran/103475 - ICE in gfc_expr_attr
! Contributed by G.Steinmetz

program p
  type t
  end type
  class(t) :: x ! { dg-error "must be dummy, allocatable or pointer" }
  y = x()       ! { dg-error "Cannot convert invalid class" }
end
