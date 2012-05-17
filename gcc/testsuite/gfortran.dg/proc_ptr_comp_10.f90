! { dg-do compile }
!
! PR 40176:  Fortran 2003: Procedure pointers with array return value
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

abstract interface
  function ai()
    real, dimension(3) :: ai
  end function
end interface

type t
  procedure(ai), pointer, nopass :: ppc
end type

procedure(ai), pointer :: pp

end module

program test
use m
type(t) :: obj
obj%ppc => pp
pp => obj%ppc
end
