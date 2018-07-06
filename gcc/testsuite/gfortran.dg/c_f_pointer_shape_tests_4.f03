! { dg-do run }
! { dg-additional-sources c_f_pointer_shape_tests_2_driver.c }
! Verify that the optional SHAPE parameter to c_f_pointer can be of any
! valid integer kind.  We don't test all kinds here since it would be 
! difficult to know what kinds are valid for the architecture we're running on.
! However, testing ones that should be different should be sufficient.
module c_f_pointer_shape_tests_4
  use, intrinsic :: iso_c_binding
  implicit none
contains
  subroutine test_long_long_1d(cPtr, num_elems) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_elems
    integer(c_int), dimension(:), pointer :: myArrayPtr
    integer(c_long_long), dimension(1) :: shape
    integer :: i
    
    shape(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 1
    end do
  end subroutine test_long_long_1d

  subroutine test_long_long_2d(cPtr, num_rows, num_cols) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_rows
    integer(c_int), value :: num_cols
    integer(c_int), dimension(:,:), pointer :: myArrayPtr
    integer(c_long_long), dimension(3) :: shape
    integer :: i,j
    
    shape(1) = num_rows
    shape(2) = -3;
    shape(3) = num_cols
    call c_f_pointer(cPtr, myArrayPtr, shape(1:3:2)) 
    do j = 1, num_cols
       do i = 1, num_rows
          if(myArrayPtr(i,j) /= ((j-1)*num_rows)+(i-1)) STOP 2
       end do
    end do
  end subroutine test_long_long_2d

  subroutine test_long_1d(cPtr, num_elems) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_elems
    integer(c_int), dimension(:), pointer :: myArrayPtr
    integer(c_long), dimension(1) :: shape
    integer :: i
    
    shape(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 3
    end do
  end subroutine test_long_1d

  subroutine test_int_1d(cPtr, num_elems) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_elems
    integer(c_int), dimension(:), pointer :: myArrayPtr
    integer(c_int), dimension(1) :: shape
    integer :: i
    
    shape(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 4
    end do
  end subroutine test_int_1d

  subroutine test_short_1d(cPtr, num_elems) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_elems
    integer(c_int), dimension(:), pointer :: myArrayPtr
    integer(c_short), dimension(1) :: shape
    integer :: i
    
    shape(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 5
    end do
  end subroutine test_short_1d

  subroutine test_mixed(cPtr, num_elems) bind(c)
    use, intrinsic :: iso_c_binding
    type(c_ptr), value :: cPtr
    integer(c_int), value :: num_elems
    integer(c_int), dimension(:), pointer :: myArrayPtr
    integer(c_int), dimension(1) :: shape1
    integer(c_long_long), dimension(1) :: shape2
    integer :: i

    shape1(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape1) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 6
    end do

    nullify(myArrayPtr)
    shape2(1) = num_elems
    call c_f_pointer(cPtr, myArrayPtr, shape2) 
    do i = 1, num_elems
       if(myArrayPtr(i) /= (i-1)) STOP 7
    end do
  end subroutine test_mixed
end module c_f_pointer_shape_tests_4
