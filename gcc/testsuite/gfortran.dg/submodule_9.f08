! { dg-do compile }
!
! Checks that the name clash between the two submodules 'mod_s' is an error.
!
! Contributed by Reinhold Bader  <reinhold.bader@lrz.de>
!
module mod_a
  implicit none
  interface
    module subroutine p()
    end subroutine
  end interface
end module

submodule (mod_a) mod_s ! { dg-error "already being used as a MODULE" }
end submodule

submodule (mod_a:mod_s) b
end submodule

submodule (mod_a:b) mod_s ! { dg-error "already being used as a MODULE" }
  implicit none
  integer :: i=-2
contains
  module procedure p
    write(*,*) 'FAIL'
  end procedure
end submodule

module mod_s
  use mod_a
  implicit none
  integer :: i=2
end module

program a_s
  use mod_s
  implicit none
  call p()
end program
! { dg-final { cleanup-submodules "mod_a@b" } }
