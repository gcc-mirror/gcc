! { dg-do run }
! { dg-additional-sources c_f_pointer_complex_driver.c }
! { dg-options "-std=gnu -w" }
! Test c_f_pointer for the different types of interoperable complex values.
module c_f_pointer_complex
  use, intrinsic :: iso_c_binding, only: c_float_complex, c_double_complex, &
       c_long_double_complex, c_f_pointer, c_ptr, c_long_double, c_int
  implicit none

contains
  subroutine test_complex_scalars(my_c_float_complex, my_c_double_complex, &
       my_c_long_double_complex) bind(c)
    type(c_ptr), value :: my_c_float_complex
    type(c_ptr), value :: my_c_double_complex
    type(c_ptr), value :: my_c_long_double_complex
    complex(c_float_complex), pointer :: my_f03_float_complex
    complex(c_double_complex), pointer :: my_f03_double_complex
    complex(c_long_double_complex), pointer :: my_f03_long_double_complex
    
    call c_f_pointer(my_c_float_complex, my_f03_float_complex)
    call c_f_pointer(my_c_double_complex, my_f03_double_complex)
    call c_f_pointer(my_c_long_double_complex, my_f03_long_double_complex)

    if(my_f03_float_complex /= (1.0, 0.0)) call abort ()
    if(my_f03_double_complex /= (2.0d0, 0.0d0)) call abort ()
    if(my_f03_long_double_complex /= (3.0_c_long_double, &
         0.0_c_long_double)) call abort ()
  end subroutine test_complex_scalars

  subroutine test_complex_arrays(float_complex_array, double_complex_array, &
       long_double_complex_array, num_elems) bind(c)
    type(c_ptr), value :: float_complex_array
    type(c_ptr), value :: double_complex_array
    type(c_ptr), value :: long_double_complex_array    
    complex(c_float_complex), pointer, dimension(:) :: f03_float_complex_array
    complex(c_double_complex), pointer, dimension(:) :: &
         f03_double_complex_array
    complex(c_long_double_complex), pointer, dimension(:) :: &
         f03_long_double_complex_array
    integer(c_int), value :: num_elems
    integer :: i

    call c_f_pointer(float_complex_array, f03_float_complex_array, &
         (/ num_elems /))
    call c_f_pointer(double_complex_array, f03_double_complex_array, &
         (/ num_elems /))
    call c_f_pointer(long_double_complex_array, &
         f03_long_double_complex_array, (/ num_elems /))

    do i = 1, num_elems
       if(f03_float_complex_array(i) &
            /= (i*(1.0, 0.0))) call abort ()
       if(f03_double_complex_array(i) &
            /= (i*(1.0d0, 0.0d0))) call abort ()
       if(f03_long_double_complex_array(i) &
            /= (i*(1.0_c_long_double, 0.0_c_long_double))) call abort ()
    end do
  end subroutine test_complex_arrays
end module c_f_pointer_complex
