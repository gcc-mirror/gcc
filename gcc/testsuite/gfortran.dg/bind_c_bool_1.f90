! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/55758
!

function sub2() bind(C) ! { dg-error "GNU Extension: LOGICAL result variable 'sub2' at .1. with non-C_Bool kind in BIND.C. procedure 'sub2'" }
  logical(kind=8) :: sub2
  logical(kind=4) :: local ! OK
end function sub2

function sub4() bind(C) result(res) ! { dg-error "GNU Extension: LOGICAL result variable 'res' at .1. with non-C_Bool kind in BIND.C. procedure 'sub4'" }
  logical(kind=2) :: res
  logical(kind=4) :: local ! OK
end function sub4


subroutine sub(x) bind(C) ! { dg-error "GNU Extension: LOGICAL dummy argument 'x' at .1. with non-C_Bool kind in BIND.C. procedure 'sub'" }
  logical(kind=2) :: x
end subroutine sub

subroutine sub3(y) bind(C)
  use iso_c_binding, only : c_bool
  logical(kind=c_bool) :: y ! OK
end subroutine sub3
