! { dg-do compile }
! Verify that the type and rank of the SHAPE argument are enforced.
module c_f_pointer_shape_tests_3
  use, intrinsic :: iso_c_binding
  
contains
  subroutine sub0(my_c_array) bind(c)
    type(c_ptr), value :: my_c_array
    integer(c_int), dimension(:), pointer :: my_array_ptr
    
    call c_f_pointer(my_c_array, my_array_ptr, (/ 10.0 /)) ! { dg-error "must be a rank 1 INTEGER array" }
  end subroutine sub0

  subroutine sub1(my_c_array) bind(c)
    type(c_ptr), value :: my_c_array
    integer(c_int), dimension(:), pointer :: my_array_ptr
    integer(c_int), dimension(1,1) :: shape

    shape(1,1) = 10
    call c_f_pointer(my_c_array, my_array_ptr, shape) ! { dg-error "must be a rank 1 INTEGER array" }
  end subroutine sub1
end module c_f_pointer_shape_tests_3
