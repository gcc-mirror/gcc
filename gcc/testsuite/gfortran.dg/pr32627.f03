! { dg-do run }
! { dg-additional-sources pr32627_driver.c }
! Verify that c_f_pointer exists for string arguments.
program main
  use iso_c_binding
  implicit none
  interface
     function get_c_string() bind(c)
       use, intrinsic :: iso_c_binding, only: c_ptr
       type(c_ptr) :: get_c_string
     end function get_c_string
  end interface

  type, bind( c ) :: A
    integer( c_int ) :: xc, yc
    type( c_ptr )    :: str
  end type
  type( c_ptr )               :: x
  type( A ), pointer          :: fptr
  type( A ), target           :: my_a_type
  character( len=9 ), pointer :: strptr

  fptr => my_a_type

  fptr%str = get_c_string()

  call c_f_pointer( fptr%str, strptr )

  print *, 'strptr is: ', strptr
end program main

  
