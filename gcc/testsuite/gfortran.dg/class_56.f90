! { dg-do compile }
! Test fix for PR56575.
!
! Contributed by A Kasahara  <latlon90180+gcc_bugzilla@gmail.com>
!
module lib_container
  implicit none

  type:: Object
  end type Object

  type:: Container
    class(Object):: v ! { dg-error "must be allocatable or pointer" }
  end type Container

contains

  subroutine proc(self)
    class(Container), intent(inout):: self
  end subroutine proc
end module lib_container

