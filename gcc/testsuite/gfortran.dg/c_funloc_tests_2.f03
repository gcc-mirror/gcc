! { dg-do compile }
module c_funloc_tests_2
  use, intrinsic :: iso_c_binding, only: c_funptr, c_funloc
  implicit none

contains
  recursive subroutine sub0() bind(c)
    type(c_funptr) :: my_c_funptr
    integer :: my_local_variable
    
    my_c_funptr = c_funloc() ! { dg-error "Missing actual argument 'x' in call to 'c_funloc'" }
    my_c_funptr = c_funloc(sub0)
    my_c_funptr = c_funloc(sub0, sub0) ! { dg-error "Too many arguments in call to 'c_funloc'" }
    my_c_funptr = c_funloc(my_local_variable) ! { dg-error "Argument X at .1. to C_FUNLOC shall be a procedure or a procedure pointer" }
  end subroutine sub0
end module c_funloc_tests_2
