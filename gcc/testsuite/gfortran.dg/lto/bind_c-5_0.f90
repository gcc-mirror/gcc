! { dg-lto-do run }
! { dg-lto-options {{ -O3 -flto }} }
! This testcase will abort if C_FUNPTR is not interoperable with both int *
! and float *
module lto_type_merge_test
  use, intrinsic :: iso_c_binding
  implicit none

  type(c_funptr), bind(c, name="myVar") :: myVar
  type(c_funptr), bind(c, name="myVar2") :: myVar2

contains
  subroutine types_test() bind(c)
    myVar = myVar2
  end subroutine types_test
end module lto_type_merge_test

