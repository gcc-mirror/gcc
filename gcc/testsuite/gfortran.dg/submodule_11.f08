! { dg-do run }
! Test the fix for PR66993, in which the use associated version of 'i'
! was incorrectly determined to be ambiguous with the 'i', host associated
! in submodule 'sm' from the module 'm'. The principle has been tested with
! the function 'time_two' in addition.
!
! Contributed by Mikael Morin  <mikael.morin@sfr.fr>
!
module m
  integer, parameter :: i = -1
  interface
    module subroutine show_i
    end subroutine show_i
  end interface
contains
  integer function times_two (arg)
    integer :: arg
    times_two = -2*arg
  end function
end module m

module n
  integer, parameter :: i = 2
contains
  integer function times_two (arg)
    integer :: arg
    times_two = 2*arg
  end function
end module n

submodule (m) sm
  use n
contains
  module subroutine show_i
    if (i .ne. 2) call abort
    if (times_two (i) .ne. 4) call abort
  end subroutine show_i
end submodule sm

program p
  use m
  call show_i
  if (i .ne. -1) call abort
  if (times_two (i) .ne. 2) call abort
end program
