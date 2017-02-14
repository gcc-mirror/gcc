! { dg-do compile }
!
! PR 43207: [OOP] invalid (pointer) assignment to and from abstract non-polymorphic expressions
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

  implicit none
  type, abstract :: parent
    integer :: i
  end type
  type, extends(parent) :: child
    class(parent), pointer :: comp
  end type

  type(child), target :: c1
  class(child), allocatable :: c2
  class(parent), pointer :: cp

  c1%parent = c1%parent  ! { dg-error "Nonpolymorphic reference to abstract type" }
  c2%parent = c1%parent  ! { dg-error "Nonpolymorphic reference to abstract type" }

  cp => c1%comp
  cp => c1%parent        ! { dg-error "Nonpolymorphic reference to abstract type" }

  call sub(c1%comp)
  call sub(c1%parent)    ! { dg-error "Nonpolymorphic reference to abstract type" }

contains

  subroutine sub(arg)
    class(parent) :: arg
  end subroutine

end
