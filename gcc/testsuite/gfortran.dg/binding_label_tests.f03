! { dg-do compile }
module binding_label_tests
  use, intrinsic :: iso_c_binding
  implicit none

  contains

  subroutine c_sub() BIND(c, name = "C_Sub")
    print *, 'hello from c_sub'
  end subroutine c_sub

  integer(c_int) function c_func() bind(C, name="__C_funC")
    print *, 'hello from c_func'
    c_func = 1
  end function c_func

  real(c_float) function f90_func() 
    print *, 'hello from f90_func'
    f90_func = 1.0
  end function f90_func

  real(c_float) function c_real_func() bind(c)
    print *, 'hello from c_real_func'
    c_real_func = 1.5
  end function c_real_func

  integer function f90_func_0() result ( f90_func_0_result ) 
    print *, 'hello from f90_func_0'
    f90_func_0_result = 0
  end function f90_func_0

  integer(c_int) function f90_func_1() result ( f90_func_1_result ) bind(c, name="__F90_Func_1__")
    print *, 'hello from f90_func_1'
    f90_func_1_result = 1
  end function f90_func_1

  integer(c_int) function f90_func_3() result ( f90_func_3_result ) bind(c)
    print *, 'hello from f90_func_3'
    f90_func_3_result = 3
  end function f90_func_3

  integer(c_int) function F90_func_2() bind(c) result ( f90_func_2_result ) 
    print *, 'hello from f90_func_2'
    f90_func_2_result = 2
  end function f90_func_2

  integer(c_int) function F90_func_4() bind(c, name="F90_func_4") result ( f90_func_4_result ) 
    print *, 'hello from f90_func_4'
    f90_func_4_result = 4
  end function f90_func_4

  integer(c_int) function F90_func_5() bind(c, name="F90_func_5") result ( f90_func_5_result ) 
    print *, 'hello from f90_func_5'
    f90_func_5_result = 5
  end function f90_func_5

  subroutine c_sub_2() bind(c, name='c_sub_2')
    print *, 'hello from c_sub_2'
  end subroutine c_sub_2

  subroutine c_sub_3() BIND(c, name = "  C_Sub_3  ")
    print *, 'hello from c_sub_3'
  end subroutine c_sub_3

  subroutine c_sub_5() BIND(c, name = "C_Sub_5        ")
    print *, 'hello from c_sub_5'
  end subroutine c_sub_5

  ! nothing between the quotes except spaces, so name="".
  ! the name will get set to the regularly mangled version of the name.  
  ! perhaps it should be marked with some characters that are invalid for 
  ! C names so C can not call it?
  subroutine sub4() BIND(c, name = "        ") 
  end subroutine sub4 
end module binding_label_tests
