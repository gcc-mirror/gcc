! { dg-do run }
! { dg-additional-sources c_loc_tests_2_funcs.c }
module c_loc_tests_2
use, intrinsic :: iso_c_binding
implicit none

interface 
   function test_scalar_address(cptr) bind(c)
     use, intrinsic :: iso_c_binding, only: c_ptr, c_int
     type(c_ptr), value :: cptr
     integer(c_int) :: test_scalar_address
   end function test_scalar_address

   function test_array_address(cptr, num_elements) bind(c)
     use, intrinsic :: iso_c_binding, only: c_ptr, c_int
     type(c_ptr), value :: cptr
     integer(c_int), value :: num_elements
     integer(c_int) :: test_array_address
   end function test_array_address

   function test_type_address(cptr) bind(c)
     use, intrinsic :: iso_c_binding, only: c_ptr, c_int
     type(c_ptr), value :: cptr
     integer(c_int) :: test_type_address
   end function test_type_address
end interface

contains
  subroutine test0() bind(c)
    integer, target :: xtar
    integer, pointer :: xptr
    type(c_ptr) :: my_c_ptr_1 = c_null_ptr
    type(c_ptr) :: my_c_ptr_2 = c_null_ptr
    xtar = 100
    xptr => xtar
    my_c_ptr_1 = c_loc(xtar)
    my_c_ptr_2 = c_loc(xptr)
    if(test_scalar_address(my_c_ptr_1) .ne. 1) then
       call abort()
    end if
    if(test_scalar_address(my_c_ptr_2) .ne. 1) then
       call abort()
    end if
  end subroutine test0

  subroutine test1() bind(c)
    integer, target, dimension(100) :: int_array_tar
    type(c_ptr) :: my_c_ptr_1 = c_null_ptr
    type(c_ptr) :: my_c_ptr_2 = c_null_ptr
    
    int_array_tar = 100
    my_c_ptr_1 = c_loc(int_array_tar)
    if(test_array_address(my_c_ptr_1, 100) .ne. 1) then
       call abort()
    end if
  end subroutine test1

  subroutine test2() bind(c)
    type, bind(c) :: f90type
       integer(c_int) :: i
       real(c_double) :: x
    end type f90type
    type(f90type), target :: type_tar
    type(f90type), pointer :: type_ptr
    type(c_ptr) :: my_c_ptr_1 = c_null_ptr
    type(c_ptr) :: my_c_ptr_2 = c_null_ptr
    
    type_ptr => type_tar
    type_tar%i = 100
    type_tar%x = 1.0d0
    my_c_ptr_1 = c_loc(type_tar)
    my_c_ptr_2 = c_loc(type_ptr)
    if(test_type_address(my_c_ptr_1) .ne. 1) then
       call abort()
    end if
    if(test_type_address(my_c_ptr_2) .ne. 1) then
       call abort()
    end if
  end subroutine test2
end module c_loc_tests_2

program driver
  use c_loc_tests_2
  call test0()
  call test1()
  call test2()
end program driver
