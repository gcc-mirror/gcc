! { dg-do run }
!
! PR fortran/48858
!
integer function foo(x)
  integer :: x
  STOP 1
  foo = 99
end function foo

integer function other() bind(C, name="bar")
  other = 42
end function other

program test
  interface
    integer function foo() bind(C, name="bar")
    end function foo
  end interface
  if (foo() /= 42) STOP 2  ! Ensure that the binding name is all what counts
end program test
