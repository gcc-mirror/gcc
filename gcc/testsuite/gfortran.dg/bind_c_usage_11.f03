! { dg-do compile }
! { dg-options "-std=gnu" }
! PR fortran/34133
!
! The compiler should accept internal procedures with BIND(c) attribute
! for STD GNU / Fortran 2008.
!
subroutine foo() bind(c)
contains
  subroutine bar() bind (c)
  end subroutine bar
end subroutine foo

subroutine foo2() bind(c)
  use iso_c_binding
contains
  integer(c_int) function barbar() bind (c)
    barbar = 1
  end function barbar
end subroutine foo2

function one() bind(c)
  use iso_c_binding
  integer(c_int) :: one
  one = 1
contains
  integer(c_int) function two() bind (c)
    two = 1
  end function two
end function one

function one2() bind(c)
  use iso_c_binding
  integer(c_int) :: one2
  one2 = 1
contains
  subroutine three() bind (c)
  end subroutine three
end function one2

program main
  use iso_c_binding
  implicit none
contains
  subroutine test() bind(c)
  end subroutine test
  integer(c_int) function test2() bind (c)
    test2 = 1
  end function test2
end program main
