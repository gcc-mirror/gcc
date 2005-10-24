! { dg-do run }

! This test is here to prevent a regression in gfc_conv_intrinsic_loc.
! Taking the loc of something in a common block was a special case
! that caused in internal compiler error in gcc/expr.c, in
! expand_expr_addr_expr_1().
program test
  common /targ/targ
  integer targ(10)
  call fn
end program test

subroutine fn
  common /targ/targ
  integer targ(10)
  call foo (loc (targ)) ! Line that caused ICE
end subroutine fn

subroutine foo (ii)
  common /targ/targ
  integer targ(10)
  integer ii
  targ(2) = ii
end subroutine foo

