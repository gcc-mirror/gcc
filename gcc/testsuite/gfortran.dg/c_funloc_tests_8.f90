! { dg-do compile }
!
! PR fortran/50612
! PR fortran/47023
!
subroutine test
  use iso_c_binding
  implicit none
  external foo
  procedure(), pointer :: pp
  print *, c_sizeof(pp) ! { dg-error "Procedure unexpected as argument" }
  print *, c_sizeof(foo) ! { dg-error "Procedure unexpected as argument" }
  print *, c_sizeof(bar) ! { dg-error "Procedure unexpected as argument" }
contains
  subroutine bar()
  end subroutine bar
end

integer function foo2()
  procedure(), pointer :: ptr
  ptr => foo2 ! { dg-error "Function result 'foo2' is invalid as proc-target in procedure pointer assignment" }
  foo2 = 7
  block
    ptr => foo2 ! { dg-error "Function result 'foo2' is invalid as proc-target in procedure pointer assignment" }
  end block
contains
  subroutine foo()
    ptr => foo2 ! { dg-error "Function result 'foo2' is invalid as proc-target in procedure pointer assignment" }
  end subroutine foo
end function foo2

module m2
contains
integer function foo(i, fptr) bind(C)
  use iso_c_binding
  implicit none
  integer :: i
  type(c_funptr) :: fptr
  fptr = c_funloc(foo) ! { dg-error "Function result 'foo' at .1. is invalid as X argument to C_FUNLOC" }
  block
    fptr = c_funloc(foo) ! { dg-error "Function result 'foo' at .1. is invalid as X argument to C_FUNLOC" }
  end block
  foo = 42*i
contains
  subroutine bar()
    fptr = c_funloc(foo) ! { dg-error "Function result 'foo' at .1. is invalid as X argument to C_FUNLOC" }
  end subroutine bar
end function foo
end module m2
