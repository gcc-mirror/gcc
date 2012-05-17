! { dg-do run }
! PR51634 - Handle allocatable components correctly in expressions 
! involving typebound operators. From comment 2 of PR but using
! classes throughout.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
! 
module soop_stars_class
  implicit none
  type soop_stars
    real, dimension(:), allocatable :: position,velocity
  contains
    procedure :: total
    procedure :: mult
    procedure :: assign
    generic :: operator(+) => total
    generic :: operator(*) => mult
    generic :: assignment(=) => assign
  end type
contains
  function mult(lhs,rhs)
    class(soop_stars) ,intent(in) :: lhs
    real ,intent(in) :: rhs
    class(soop_stars), allocatable :: mult
    type(soop_stars) :: tmp
    tmp = soop_stars (lhs%position*rhs, lhs%velocity*rhs)
    allocate (mult, source = tmp)
  end function

  function total(lhs,rhs)
    class(soop_stars) ,intent(in) :: lhs,rhs
    class(soop_stars), allocatable :: total
    type(soop_stars) :: tmp
    tmp = soop_stars (lhs%position + rhs%position, &
                      lhs%velocity + rhs%velocity)
    allocate (total, source = tmp)
  end function

  subroutine assign(lhs,rhs)
    class(soop_stars), intent(in) :: rhs
    class(soop_stars), intent(out) :: lhs
    lhs%position = rhs%position
    lhs%velocity = rhs%velocity
  end subroutine
end module

program main
  use soop_stars_class ,only : soop_stars
  implicit none
  class(soop_stars), allocatable :: fireworks
  real :: dt
  allocate (fireworks, source = soop_stars ([1,2,3], [4,5,6]))
  dt = 5
  fireworks = fireworks + fireworks*dt
  if (any (fireworks%position .ne. [6, 12, 18])) call abort
  if (any (fireworks%velocity .ne. [24, 30, 36])) call abort
end program
