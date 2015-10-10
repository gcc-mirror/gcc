! { dg-lto-do run }
! { dg-lto-options {{ -O3 -flto }} }
! This testcase will abort if real/complex/boolean/character types are not interoperable
module lto_type_merge_test
  use, intrinsic :: iso_c_binding
  implicit none

  type, bind(c) :: MYFTYPE_1
    real(c_float) :: val_1
    real(c_double) :: val_2
    real(c_long_double) :: val_3
    complex(c_float_complex) :: val_4
    complex(c_double_complex) :: val_5
    complex(c_long_double_complex) :: val_6
    logical(c_bool) :: val_7
    !FIXME: Fortran define c_char as array of size 1.
    !character(c_char) :: val_8
  end type MYFTYPE_1

  type(myftype_1), bind(c, name="myVar") :: myVar

contains
  subroutine types_test1() bind(c)
    myVar%val_1 = 2
  end subroutine types_test1
  subroutine types_test2() bind(c)
    myVar%val_2 = 2
  end subroutine types_test2
  subroutine types_test3() bind(c)
    myVar%val_3 = 2
  end subroutine types_test3
  subroutine types_test4() bind(c)
    myVar%val_4 = 2
  end subroutine types_test4
  subroutine types_test5() bind(c)
    myVar%val_5 = 2
  end subroutine types_test5
  subroutine types_test6() bind(c)
    myVar%val_6 = 2
  end subroutine types_test6
  subroutine types_test7() bind(c)
    myVar%val_7 = myVar%val_7 .or. .not. myVar%val_7
  end subroutine types_test7
  !subroutine types_test8() bind(c)
    !myVar%val_8 = "a"
  !end subroutine types_test8
end module lto_type_merge_test

