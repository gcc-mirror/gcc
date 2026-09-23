! { dg-do run }
! { dg-compile-aux-modules "submodule_35.f90" }
! { dg-additional-sources submodule_35.f90 }
!
submodule(parent) decendent
contains
  module subroutine my_method(this)
    class(my_type), intent(inout) :: this
    this%i = my_number()
  end subroutine my_method
end submodule decendent

program main
  use parent
  type(my_type) :: my
  call my % my_method()
  if (my%i /= 42) stop 1
end program main
! { dg-final { cleanup-modules "parent" } }
