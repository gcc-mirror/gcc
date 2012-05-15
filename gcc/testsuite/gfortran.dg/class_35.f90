! { dg-do run }
!
! PR 46313: [OOP] class container naming collisions
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module one
  type two_three
  end type
end module

module one_two
  type three
  end type
end module

use one
use one_two
class(two_three), allocatable :: a1
class(three), allocatable :: a2

if (same_type_as(a1,a2)) call abort()

end 
