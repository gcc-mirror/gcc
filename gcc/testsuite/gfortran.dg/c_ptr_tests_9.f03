! { dg-do run }
! { dg-options "-std=gnu" }
! This test is pretty simple but is here just to make sure that the changes 
! done to c_ptr and c_funptr (translating them to void *) works in the case 
! where a component of a type is of type c_ptr or c_funptr.  
module c_ptr_tests_9
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr

  type myF90Derived
     type(c_ptr) :: my_c_ptr
  end type myF90Derived

contains
  subroutine sub0() bind(c)
    type(myF90Derived), target :: my_f90_type
    type(myF90Derived), pointer :: my_f90_type_ptr

    my_f90_type%my_c_ptr = c_null_ptr
    print *, 'my_f90_type is: ', my_f90_type%my_c_ptr
    my_f90_type_ptr => my_f90_type
    print *, 'my_f90_type_ptr is: ', my_f90_type_ptr%my_c_ptr
  end subroutine sub0
end module c_ptr_tests_9


program main
  use c_ptr_tests_9

  call sub0()
end program main
