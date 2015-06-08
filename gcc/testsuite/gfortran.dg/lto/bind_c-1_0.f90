! { dg-lto-do run }
! { dg-lto-options {{ -O3 -flto }} }
! This testcase will abort if C_PTR is not interoperable with both int *
! and float *
module lto_type_merge_test
  use, intrinsic :: iso_c_binding
  implicit none

  type, bind(c) :: MYFTYPE_1
     type(c_ptr) :: ptr
     type(c_ptr) :: ptrb
  end type MYFTYPE_1

  type(myftype_1), bind(c, name="myVar") :: myVar

contains
  subroutine types_test() bind(c)
    myVar%ptr = myVar%ptrb
  end subroutine types_test
end module lto_type_merge_test

