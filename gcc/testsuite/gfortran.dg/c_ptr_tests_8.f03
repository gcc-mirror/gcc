! { dg-do run }
! { dg-additional-sources c_ptr_tests_8_funcs.c }
program main
use iso_c_binding, only: c_ptr
implicit none
interface
  function create() bind(c)
    use iso_c_binding, only: c_ptr
    type(c_ptr) :: create
  end function create
  subroutine show(a) bind(c)
    import :: c_ptr
    type(c_ptr), VALUE :: a
  end subroutine show
end interface

type(c_ptr) :: ptr
ptr = create()
call show(ptr)
end program main
