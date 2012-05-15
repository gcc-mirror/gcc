! { dg-do run }
! This test case simply checks that c_funloc exists, accepts arguments of 
! flavor FL_PROCEDURE, and returns the type c_funptr
module c_funloc_tests
  use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc

contains
  recursive subroutine sub0() bind(c)
    type(c_funptr) :: my_c_funptr

    my_c_funptr = c_funloc(sub0)
  end subroutine sub0
end module c_funloc_tests

program driver
  use c_funloc_tests
  
  call sub0()
end program driver
