! { dg-do run }
!
! Checks that F2008:11.2.3 para 2 is correctly implemented so that
! no error results from using 'mod_s' for both a module name and
! a submodule name. The submodule is now identified as 'mod_a.mod_s'
! internally and the submodule file as 'mod_a@mod_s.smod'.
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

submodule (mod_a) mod_s
  implicit none
  integer :: i=-2
contains
  module procedure p
    if (i .ne. -2) then
      call abort
    end if
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
  if (i==2) then
    call p()
  else
    call abort
  end if
end program
! { dg-final { cleanup-submodules "mod_a@mod_s" } }
