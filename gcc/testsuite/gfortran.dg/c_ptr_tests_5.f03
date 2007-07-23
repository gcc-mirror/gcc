! { dg-do compile }
module c_ptr_tests_5
use, intrinsic :: iso_c_binding

type, bind(c) :: my_f90_type
   integer(c_int) :: i
end type my_f90_type

contains
  subroutine sub0(c_struct) bind(c)
    type(c_ptr), value :: c_struct
    type(my_f90_type) :: f90_type

    call c_f_pointer(c_struct, f90_type) ! { dg-error "must be a pointer" }
  end subroutine sub0
end module c_ptr_tests_5
