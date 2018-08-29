! { dg-do run }
!
! Tests the fix for PR79676 in which submod_test was private even to the
! submodule 'my_submod'.
!
! Contributed by Adam Hirst  <adam@aphirst.karoo.co.uk>
!
module my_mod
  private           ! This hid 'submod_test'.
  interface
    module subroutine submod_test(x)
      integer :: x
    end subroutine
  end interface
  integer answer
  public routine1, print_two, answer
contains
  subroutine routine1(x)
    integer :: x
    call submod_test(x)
  end subroutine
  subroutine print_two()
    integer, parameter :: two = 2
    answer = answer * two
  end subroutine
end module

module my_mod_2
  use my_mod
contains
  subroutine circular_dependency()
    call print_two()
  end subroutine
end module

submodule (my_mod) my_submod
  use my_mod_2
contains
module subroutine submod_test(x)
  integer :: x
  answer = x
  call circular_dependency()
end subroutine

end submodule

program hello
  use my_mod
  implicit none
  call routine1(2)
  if (answer .ne. 4) STOP 1
end program
