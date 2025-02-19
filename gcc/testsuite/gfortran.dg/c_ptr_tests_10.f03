! { dg-do run }
! This test case exists because gfortran had an error in converting the 
! expressions for the derived types from iso_c_binding in some cases.
module c_ptr_tests_10
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_intptr_t

contains
  subroutine sub0() bind(c)
    print *, 'c_null_ptr is: ', transfer (cptr, 0_C_INTPTR_T)
  end subroutine sub0
end module c_ptr_tests_10

program main
  use c_ptr_tests_10
  call sub0()
end program main
