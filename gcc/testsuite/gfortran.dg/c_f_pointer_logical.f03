! { dg-do run }
! { dg-additional-sources c_f_pointer_logical_driver.c }
! Verify that c_f_pointer exists for C logicals (_Bool).
module c_f_pointer_logical
  use, intrinsic :: iso_c_binding, only: c_bool, c_f_pointer, c_ptr, c_int
contains
  subroutine test_scalar(c_logical_ptr) bind(c)
    type(c_ptr), value :: c_logical_ptr
    logical(c_bool), pointer :: f03_logical_ptr
    call c_f_pointer(c_logical_ptr, f03_logical_ptr)
    
    if(f03_logical_ptr .neqv. .true.) call abort ()
  end subroutine test_scalar

  subroutine test_array(c_logical_array, num_elems) bind(c)
    type(c_ptr), value :: c_logical_array
    integer(c_int), value :: num_elems
    logical(c_bool), pointer, dimension(:) :: f03_logical_array
    integer :: i

    call c_f_pointer(c_logical_array, f03_logical_array, (/ num_elems /))

    ! Odd numbered locations are true (even numbered offsets in C)
    do i = 1, num_elems, 2
       if(f03_logical_array(i) .neqv. .true.) call abort ()
    end do
    
    ! Even numbered locations are false.
    do i = 2, num_elems, 2
       if(f03_logical_array(i) .neqv. .false.) call abort ()
    end do
  end subroutine test_array
end module c_f_pointer_logical
