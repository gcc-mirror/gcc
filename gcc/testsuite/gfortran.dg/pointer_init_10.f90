! { dg-do run }
! { dg-require-visibility "" }
!
! PR 84504: [F08] procedure pointer variables cannot be initialized with functions returning pointers
!
! Contributed by Sriram Swaminarayan <sriram@pobox.com>

module test_mod
  implicit none
  private
  integer, target :: i = 333
  procedure(the_proc), pointer, public  :: ptr => the_proc
contains
  function the_proc() 
    integer, pointer :: the_proc
    the_proc => i
  end function
end module

program test_prog
  use test_mod
  integer, pointer :: ip
  ip => ptr()
  if (ip /= 333) stop 1
end
