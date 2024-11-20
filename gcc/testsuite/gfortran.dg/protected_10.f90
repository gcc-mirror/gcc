! { dg-do compile }
! PR fortran/83135 - fix checking of protected variables in submodules

module mod1
  implicit none
  private
  integer, protected, public :: xx = 42
  public :: set_xx
  public :: echo1_xx, echo2_xx
  interface
     module subroutine echo1_xx()
     end subroutine echo1_xx
     module subroutine echo2_xx()
     end subroutine echo2_xx
  end interface
contains
  subroutine set_xx(arg)
    integer, intent(in) :: arg
    xx = arg    ! valid (it is host_associated)
  end
end module
!
submodule (mod1) s1mod1
  implicit none
contains
  module subroutine echo1_xx()
    xx = 11     ! valid (it is from the ancestor)
    write(*,*) "xx=", xx
  end subroutine echo1_xx
end submodule
!
submodule (mod1:s1mod1) s2mod1
  implicit none
contains
  module subroutine echo2_xx()
    xx = 12     ! valid (it is from the ancestor)
    write(*,*) "xx=", xx
  end subroutine echo2_xx
end submodule
!
module mod2
  use mod1
  implicit none
  integer, protected, public :: yy = 43
  interface
     module subroutine echo_xx()
     end subroutine echo_xx
  end interface
contains
  subroutine bla
!   xx = 999    ! detected, leads to fatal error
  end
end module
!
submodule (mod2) smod2
  implicit none
contains
  module subroutine echo_xx ()
    xx = 10     ! { dg-error "is PROTECTED" }
    write(*,*) "xx=", xx
    yy = 22     ! valid (it is from the ancestor)
  end
end submodule
!
program test_protected
  use mod1
  use mod2
  implicit none
  write(*,*) "xx=", xx
  call set_xx(88)
  write(*,*) "xx=", xx
  call echo_xx
  call echo1_xx
  call echo2_xx
end program
