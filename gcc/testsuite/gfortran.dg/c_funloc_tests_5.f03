! { dg-do compile }
! { dg-options "-std=f2003" }
! Test that the arg checking for c_funloc verifies the procedures are 
! C interoperable.
module c_funloc_tests_5
  use, intrinsic :: iso_c_binding, only: c_funloc, c_funptr
contains
  subroutine sub0() bind(c)
    type(c_funptr) :: my_c_funptr

    my_c_funptr = c_funloc(sub1) ! { dg-error "TS 29113: Noninteroperable procedure at .1. to C_FUNLOC" }

    my_c_funptr = c_funloc(func0) ! { dg-error "TS 29113: Noninteroperable procedure at .1. to C_FUNLOC" }
  end subroutine sub0

  subroutine sub1() 
  end subroutine sub1

  function func0(desired_retval) 
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: desired_retval
    integer(c_int) :: func0
    func0 = desired_retval
  end function func0
end module c_funloc_tests_5


