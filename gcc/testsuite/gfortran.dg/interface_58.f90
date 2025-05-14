! { dg-do compile }
! PR 119078 - there should be no warning for dummy arguments
! or abstract interfaces.
module x
  implicit none
  abstract interface
     subroutine foo() bind(c)
     end subroutine foo
  end interface
  interface
     subroutine baz() bind(c) ! { dg-warning "wrong number of arguments" }
     end subroutine baz
  end interface
contains
  subroutine tescht(bar) bind(c)
    interface
       subroutine bar() bind(c)
       end subroutine bar
    end interface
  end subroutine tescht
  subroutine t2(bar) bind(c)
    procedure(foo) :: bar
  end subroutine t2
end module x

subroutine foo(a)
  real :: a
end subroutine foo

subroutine bar(b)
  real :: b
end subroutine bar

subroutine baz(a) bind(c) ! { dg-warning "wrong number of arguments" }
  use iso_c_binding, only : c_int
  integer(c_int) :: a
end subroutine baz
  
