! { dg-do compile }
! Verify that the compiler catches the error in the call to c_f_pointer 
! because it is missing the required SHAPE argument. The SHAPE argument 
! is optional, in general, but must exist if given a Fortran pointer 
! to a non-zero rank object.  --Rickett, 09.26.06
module c_f_pointer_shape_test
contains
  subroutine test_0(myAssumedArray, cPtr)
    use, intrinsic :: iso_c_binding
    integer, dimension(*) :: myAssumedArray
    integer, dimension(:), pointer :: myArrayPtr
    integer, dimension(1:2), target :: myArray
    type(c_ptr), value :: cPtr
    
    myArrayPtr => myArray
    call c_f_pointer(cPtr, myArrayPtr) ! { dg-error "Expected SHAPE argument to C_F_POINTER with array FPTR" }
  end subroutine test_0
end module c_f_pointer_shape_test
