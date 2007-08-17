! { dg-do run }
! { dg-additional-sources test_c_assoc.c }
module c_assoc
  use, intrinsic :: iso_c_binding
  implicit none

contains

  function test_c_assoc_0(my_c_ptr) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_associated
    integer(c_int) :: test_c_assoc_0
    type(c_ptr), value :: my_c_ptr

    if(c_associated(my_c_ptr)) then
       test_c_assoc_0 = 1
    else
       test_c_assoc_0 = 0
    endif
  end function test_c_assoc_0

  function test_c_assoc_1(my_c_ptr_1, my_c_ptr_2) bind(c)
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_associated
    integer(c_int) :: test_c_assoc_1
    type(c_ptr), value :: my_c_ptr_1
    type(c_ptr), value :: my_c_ptr_2

    if(c_associated(my_c_ptr_1, my_c_ptr_2)) then
       test_c_assoc_1 = 1
    else
       test_c_assoc_1 = 0
    endif
  end function test_c_assoc_1

  function test_c_assoc_2(my_c_ptr_1, my_c_ptr_2, num_ptrs) bind(c)
    integer(c_int) :: test_c_assoc_2
    type(c_ptr), value :: my_c_ptr_1
    type(c_ptr), value :: my_c_ptr_2
    integer(c_int), value :: num_ptrs
    
    if(num_ptrs .eq. 1) then
       if(c_associated(my_c_ptr_1)) then
          test_c_assoc_2 = 1
       else
          test_c_assoc_2 = 0
       endif
    else
       if(c_associated(my_c_ptr_1, my_c_ptr_2)) then
          test_c_assoc_2 = 1
       else
          test_c_assoc_2 = 0
       endif
    endif
  end function test_c_assoc_2

  subroutine verify_assoc(my_c_ptr_1, my_c_ptr_2) bind(c)
    type(c_ptr), value :: my_c_ptr_1
    type(c_ptr), value :: my_c_ptr_2

    if(.not. c_associated(my_c_ptr_1)) then
       call abort()
    else if(.not. c_associated(my_c_ptr_2)) then
       call abort()
    else if(.not. c_associated(my_c_ptr_1, my_c_ptr_2)) then
       call abort()
    endif
  end subroutine verify_assoc
  
end module c_assoc

! { dg-final { cleanup-modules "c_assoc" } }
