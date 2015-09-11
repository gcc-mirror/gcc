! { dg-do run }
!
! PR fortran/39427
!
module foo_module
  interface foo
    procedure constructor
  end interface

  type foo
    integer :: bar
  end type
contains
  type(foo) function constructor()
    constructor%bar = 1
  end function

  subroutine test_foo()
    type(foo) :: f
    f = foo()
    if (f%bar /= 1) call abort ()
    f = foo(2)
    if (f%bar /= 2) call abort ()
  end subroutine test_foo
end module foo_module


! Same as foo_module but order
! of INTERFACE and TYPE reversed
module bar_module
  type bar
    integer :: bar
  end type

  interface bar
    procedure constructor
  end interface
contains
  type(bar) function constructor()
    constructor%bar = 3
  end function

  subroutine test_bar()
    type(bar) :: f
    f = bar()
    if (f%bar /= 3) call abort ()
    f = bar(4)
    if (f%bar /= 4) call abort ()
  end subroutine test_bar
end module bar_module

program main
  use foo_module
  use bar_module
  implicit none

  type(foo) :: f
  type(bar) :: b

  call test_foo()
  f = foo()
  if (f%bar /= 1) call abort ()
  f = foo(2)
  if (f%bar /= 2) call abort ()

  call test_bar()
  b = bar()
  if (b%bar /= 3) call abort ()
  b = bar(4)
  if (b%bar /= 4) call abort ()
end program main

