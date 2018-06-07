! { dg-do compile }
! PR fortran/64107
! Code contribute by  fxcoudert at gcc dot gnu dot org
! Appears to be fixed by patch for PR fortran/83633
module m1

contains
  pure integer function foo()
    foo = 2
  end function
end module

subroutine test
  use m1
  integer :: x1(foo())
end subroutine

module m
  use m1
  integer :: x2(foo())     ! { dg-error "array with nonconstant bounds" }
contains
  subroutine sub
    integer :: x3(foo())
  end subroutine
end module

program p
  use m1
  integer :: x4(foo())     ! { dg-error "array with nonconstant bounds" }
end program
