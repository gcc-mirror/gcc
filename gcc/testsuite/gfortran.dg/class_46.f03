! { dg-do run }
!
! PR 50625: [4.6/4.7 Regression][OOP] ALLOCATABLE attribute lost for module CLASS variables
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
type t
end type t
class(t), allocatable :: x
end module m

use m
implicit none
if (allocated(x)) STOP 1
end 
