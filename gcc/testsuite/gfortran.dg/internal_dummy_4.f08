! { dg-do run }
! PR fortran/34133
! PR fortran/34162
!
! Test of using internal bind(C) procedures as
! actual argument. Bind(c) on internal procedures and
! internal procedures are actual argument are
! Fortran 2008 (draft) extension.
!
module test_mod
  use iso_c_binding
  implicit none
contains
  subroutine test_sub(a, arg, res)
    interface
      subroutine a(x) bind(C)
        import
        integer(c_int), intent(inout) :: x
      end subroutine a
    end interface
    integer(c_int), intent(inout) :: arg
    integer(c_int), intent(in) :: res
    call a(arg)
    if(arg /= res) STOP 1
  end subroutine test_sub
  subroutine test_func(a, arg, res)
    interface
      integer(c_int) function a(x) bind(C)
        import
        integer(c_int), intent(in) :: x
      end function a
    end interface
    integer(c_int), intent(in) :: arg
    integer(c_int), intent(in) :: res
    if(a(arg) /= res) STOP 2
  end subroutine test_func
end module test_mod

program main
  use test_mod
  implicit none
  integer :: a
  a = 33
  call test_sub (one, a, 7*33)
  a = 23
  call test_func(two, a, -123*23)
contains
  subroutine one(x) bind(c)
     integer(c_int),intent(inout) :: x
     x = 7*x
  end subroutine one
  integer(c_int) function two(y) bind(c)
     integer(c_int),intent(in) :: y
     two = -123*y
  end function two
end program main
