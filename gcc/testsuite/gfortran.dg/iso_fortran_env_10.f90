! { dg-do run }
!
! PR fortran/82065
!
! Array-valued named constants from ISO_FORTRAN_ENV were emitted without
! a static initializer when used inside a procedure.

module m
contains
  subroutine mod_proc
    use iso_fortran_env, only : integer_kinds
    if (size (integer_kinds) < 1) stop 1
    if (integer_kinds(1) < 1) stop 2
  end subroutine
end module

program main
  use m
  use iso_fortran_env
  implicit none

  call check (integer_kinds)
  call testsub
  call testsub2
  call mod_proc

contains

  subroutine check (k)
    integer, intent(in) :: k(:)
    if (size (k) < 1) stop 3
    if (any (k < 1)) stop 4
  end subroutine

  subroutine testsub
    if (size (integer_kinds) < 1) stop 5
    if (any (integer_kinds < 1)) stop 6
    call check (integer_kinds)
  end subroutine

  subroutine testsub2
    use iso_fortran_env, only : x => integer_kinds, y => real_kinds, &
                                z => logical_kinds, c => character_kinds
    if (any (x < 1)) stop 7
    if (any (y < 1)) stop 8
    if (any (z < 1)) stop 9
    if (any (c < 1)) stop 10
  end subroutine

end program main
