! { dg-do compile }
! Test the fix for PR79434 in which the PRIVATE attribute of the
! component 'i' of the derived type 't' was not respected in the
! submodule 's_u'.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module mod_encap_t
  implicit none
  type, public :: t
    private
    integer :: i
  end type
end module
module mod_encap_u
  use mod_encap_t
  type, public, extends(t) :: u
    private
    integer :: j
  end type
  interface
    module subroutine fu(this)
      type(u), intent(inout) :: this
    end subroutine
  end interface
end module
submodule (mod_encap_u) s_u
contains
  module procedure fu
!   the following statement should cause the compiler to
!   abort, pointing out a private component defined in
!   a USED module is being accessed
    this%i = 2 ! { dg-error "is a PRIVATE component" }
    this%j = 1
    write(*, *) 'FAIL'
  end procedure
end submodule
program p
  use mod_encap_u
  implicit none
  type(u) :: x
  call fu(x)
end program
