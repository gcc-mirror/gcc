! { dg-do compile }
! { dg-options "-std=f2023" }
! Verify that the type and rank of the LOWER argument are enforced.
module c_f_pointer_shape_tests_8
  use, intrinsic :: iso_c_binding

contains
  subroutine sub2(my_c_array) bind(c)
    type(c_ptr), value :: my_c_array
    integer(kind=c_int), dimension(:), pointer :: my_array_ptr

    call c_f_pointer(my_c_array, my_array_ptr, (/ 10 /), (/ 10.0 /)) ! { dg-error "must be INTEGER" }
  end subroutine sub2

  subroutine sub3(my_c_array) bind(c)
    type(c_ptr), value :: my_c_array
    integer(kind=c_int), dimension(:), pointer :: my_array_ptr
    integer(kind=c_int), dimension(1) :: shape
    integer(kind=c_int), dimension(1, 1) :: lower

    lower(1, 1) = 10
    call c_f_pointer(my_c_array, my_array_ptr, shape, lower) ! { dg-error "must be of rank 1" }
  end subroutine sub3
end module c_f_pointer_shape_tests_8
