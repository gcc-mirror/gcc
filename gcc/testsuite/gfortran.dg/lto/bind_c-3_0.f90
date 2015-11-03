! { dg-lto-do run }
! { dg-lto-options {{ -O3 -flto }} }
! This testcase will abort if integer types are not interoperable.
module lto_type_merge_test
  use, intrinsic :: iso_c_binding
  implicit none

  type, bind(c) :: MYFTYPE_1
    integer(c_int) :: val_int
    integer(c_short) :: val_short
    integer(c_long) :: val_long
    integer(c_long_long) :: val_long_long
    integer(c_size_t) :: val_size_t
    integer(c_int8_t) :: val_int8_t
    integer(c_int16_t) :: val_int16_t
    integer(c_int32_t) :: val_int32_t
    integer(c_int64_t) :: val_int64_t
    integer(c_int_least8_t) :: val_intleast_8_t
    integer(c_int_least16_t) :: val_intleast_16_t
    integer(c_int_least32_t) :: val_intleast_32_t
    integer(c_int_least64_t) :: val_intleast_64_t
    integer(c_int_fast8_t) :: val_intfast_8_t
    integer(c_int_fast16_t) :: val_intfast_16_t
    integer(c_int_fast32_t) :: val_intfast_32_t
    integer(c_int_fast64_t) :: val_intfast_64_t
    integer(c_intmax_t) :: val_intmax_t
    integer(c_intptr_t) :: val_intptr_t
  end type MYFTYPE_1

  type(myftype_1), bind(c, name="myVar") :: myVar

contains
  subroutine types_test1() bind(c)
    myVar%val_int = 2
  end subroutine types_test1
  subroutine types_test2() bind(c)
    myVar%val_short = 2
  end subroutine types_test2
  subroutine types_test3() bind(c)
    myVar%val_long = 2
  end subroutine types_test3
  subroutine types_test4() bind(c)
    myVar%val_long_long = 2
  end subroutine types_test4
  subroutine types_test5() bind(c)
    myVar%val_size_t = 2
  end subroutine types_test5
  subroutine types_test6() bind(c)
    myVar%val_int8_t = 2
  end subroutine types_test6
  subroutine types_test7() bind(c)
    myVar%val_int16_t = 2
  end subroutine types_test7
  subroutine types_test8() bind(c)
    myVar%val_int32_t = 2
  end subroutine types_test8
  subroutine types_test9() bind(c)
    myVar%val_int64_t = 2
  end subroutine types_test9
  subroutine types_test10() bind(c)
    myVar%val_intleast_8_t = 2
  end subroutine types_test10
  subroutine types_test11() bind(c)
    myVar%val_intleast_16_t = 2
  end subroutine types_test11
  subroutine types_test12() bind(c)
    myVar%val_intleast_32_t = 2
  end subroutine types_test12
  subroutine types_test13() bind(c)
    myVar%val_intleast_64_t = 2
  end subroutine types_test13
  subroutine types_test14() bind(c)
    myVar%val_intfast_8_t = 2
  end subroutine types_test14
  subroutine types_test15() bind(c)
    myVar%val_intfast_16_t = 2
  end subroutine types_test15
  subroutine types_test16() bind(c)
    myVar%val_intfast_32_t = 2
  end subroutine types_test16
  subroutine types_test17() bind(c)
    myVar%val_intfast_64_t = 2
  end subroutine types_test17
  subroutine types_test18() bind(c)
    myVar%val_intmax_t = 2
  end subroutine types_test18
  subroutine types_test19() bind(c)
    myVar%val_intptr_t = 2
  end subroutine types_test19
end module lto_type_merge_test

