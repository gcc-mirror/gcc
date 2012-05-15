! { dg-do run }
! { dg-additional-sources c_char_driver.c }
! Verify that character dummy arguments for bind(c) procedures can work both 
! by-value and by-reference when called by either C or Fortran.
! PR fortran/32732
module c_char_tests
  use, intrinsic :: iso_c_binding, only: c_char
  implicit none
contains
  subroutine param_test(my_char, my_char_2) bind(c)
    character(c_char), value :: my_char
    character(c_char), value :: my_char_2
    if(my_char /= c_char_'y') call abort()
    if(my_char_2 /= c_char_'z') call abort()
    
    call sub1(my_char)
  end subroutine param_test

  subroutine sub0() bind(c)
    call param_test('y', 'z')
  end subroutine sub0

  subroutine sub1(my_char_ref) bind(c)
    character(c_char) :: my_char_ref
    if(my_char_ref /= c_char_'y') call abort()
  end subroutine sub1
end module c_char_tests
