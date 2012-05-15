! { dg-do run }
! { dg-additional-sources c_funloc_tests_4_driver.c }
! Test that the inlined c_funloc works.
module c_funloc_tests_4
  use, intrinsic :: iso_c_binding, only: c_funloc, c_funptr
  interface
     subroutine c_sub0(fsub_ptr) bind(c)
       use, intrinsic :: iso_c_binding, only: c_funptr
       type(c_funptr), value :: fsub_ptr
     end subroutine c_sub0
     subroutine c_sub1(ffunc_ptr) bind(c)
       use, intrinsic :: iso_c_binding, only: c_funptr
       type(c_funptr), value :: ffunc_ptr
     end subroutine c_sub1
  end interface
contains
  subroutine sub0() bind(c)
    type(c_funptr) :: my_c_funptr

    my_c_funptr = c_funloc(sub1)
    call c_sub0(my_c_funptr)

    my_c_funptr = c_funloc(func0)
    call c_sub1(my_c_funptr)
  end subroutine sub0

  subroutine sub1() bind(c)
    print *, 'hello from sub1'
  end subroutine sub1

  function func0(desired_retval) bind(c)
    use, intrinsic :: iso_c_binding, only: c_int
    integer(c_int), value :: desired_retval
    integer(c_int) :: func0
    print *, 'hello from func0'
    func0 = desired_retval
  end function func0
end module c_funloc_tests_4
