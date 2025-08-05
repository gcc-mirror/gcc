! { dg-do run }
!
! Check the span of the descriptor of an array pointer after it has been
! assigned to from a polymorphic function result.

program test
  implicit none
  type t
    integer :: c
  end type t
  type, extends(t) :: u
    integer :: d
  end type u
  type(t), pointer :: p(:)
  class(t), allocatable, target :: a(:)
  p => f()
  ! print *, p%c
  if (any(p%c /= [2,5,11,17,23])) error stop 1
contains
  function f()
    class(t), pointer :: f(:)
    a = [ u(2,3), u(5,7), u(11,13), u(17,19), u(23,29) ] 
    f => a
  end function
end program
