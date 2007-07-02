! { dg-do run }
! { dg-additional-sources c_funloc_tests_3_funcs.c }
! This testcase tests c_funloc and c_funptr from iso_c_binding.  It uses 
! functions defined in c_funloc_tests_3_funcs.c.
module c_funloc_tests_3
 implicit none
contains
  function ffunc(j) bind(c)
    use iso_c_binding, only: c_funptr, c_int
    integer(c_int)        :: ffunc
    integer(c_int), value :: j
    ffunc = -17*j
  end function ffunc
end module c_funloc_tests_3
program main
  use iso_c_binding, only: c_funptr, c_funloc
  use c_funloc_tests_3, only: ffunc
  implicit none
  interface
    function returnFunc() bind(c,name="returnFunc")
       use iso_c_binding, only: c_funptr
       type(c_funptr) :: returnFunc
    end function returnFunc
    subroutine callFunc(func,pass,compare) bind(c,name="callFunc")
       use iso_c_binding, only: c_funptr, c_int
       type(c_funptr), value :: func
       integer(c_int), value :: pass,compare
    end subroutine callFunc
  end interface
  type(c_funptr) :: p
  p = returnFunc()
  call callFunc(p, 13,3*13)
  p = c_funloc(ffunc)
  call callFunc(p, 21,-17*21)
end program main
! { dg-final { cleanup-modules "c_funloc_tests_3" } }
