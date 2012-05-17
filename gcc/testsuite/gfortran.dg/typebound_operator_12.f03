! { dg-do run }
! PR51634 - Handle allocatable components correctly in expressions 
! involving typebound operators. See comment 2 of PR.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
! 
module soop_stars_class
  implicit none
  type soop_stars
    real, dimension(:), allocatable :: position,velocity
  contains
    procedure :: total
    procedure :: product
    generic :: operator(+) => total
    generic :: operator(*) => product
  end type
contains
  type(soop_stars) function product(lhs,rhs)
    class(soop_stars) ,intent(in) :: lhs
    real ,intent(in) :: rhs
    product%position = lhs%position*rhs
    product%velocity = lhs%velocity*rhs
  end function

  type(soop_stars) function total(lhs,rhs)
    class(soop_stars) ,intent(in) :: lhs,rhs
    total%position = lhs%position + rhs%position
    total%velocity = lhs%velocity + rhs%velocity
  end function
end module

program main
  use soop_stars_class ,only : soop_stars
  implicit none
  type(soop_stars) :: fireworks
  real :: dt
  fireworks%position = [1,2,3]
  fireworks%velocity = [4,5,6]
  dt = 5
  fireworks = fireworks + fireworks*dt
  if (any (fireworks%position .ne. [6, 12, 18])) call abort
  if (any (fireworks%velocity .ne. [24, 30, 36])) call abort
end program
