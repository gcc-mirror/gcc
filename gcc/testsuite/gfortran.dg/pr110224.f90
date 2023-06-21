! { dg-do compile }
!
! Contributed by Neil Carlson  <neil.n.carlson@gmail.com>
!
module mod
  type :: foo
    real, pointer :: var
  contains
    procedure :: var_ptr
  end type
contains
  function var_ptr(this) result(ref)
    class(foo) :: this
    real, pointer :: ref
    ref => this%var
  end function
end module
program main
  use mod
  type(foo) :: x
  allocate (x%var, source = 2.0)
  associate (var => x%var_ptr())
    var = 1.0
  end associate
  if (x%var .ne. 1.0) stop 1
  x%var_ptr() = 2.0
  if (x%var .ne. 2.0) stop 2
  deallocate (x%var)
end program
