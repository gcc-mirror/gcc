! { dg-do compile }
module c_funloc_tests_2
  use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
  implicit none

contains
  subroutine sub0() bind(c)
    type(c_funptr) :: my_c_funptr
    integer :: my_local_variable
    
    my_c_funptr = c_funloc() ! { dg-error "Missing argument" }
    my_c_funptr = c_funloc(sub0)
    my_c_funptr = c_funloc(sub0, sub0) ! { dg-error "More actual than formal" }
    my_c_funptr = c_funloc(my_local_variable) ! { dg-error "must be a procedure" }
  end subroutine sub0
end module c_funloc_tests_2
