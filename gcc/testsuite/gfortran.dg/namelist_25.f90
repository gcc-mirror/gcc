! { dg-do compile }
! Tests patch for PR29407, in which the declaration of 'my' as
! a local variable was ignored, so that the procedure and namelist
! attributes for 'my' clashed..
!
! Contributed by Tobias Burnus  <tobias.burnus@physik.fu-berlin.de>
!
program main
  implicit none
contains
  subroutine my
  end subroutine my
  subroutine bar
    integer :: my
    namelist /ops/ my
  end subroutine bar
end program main

