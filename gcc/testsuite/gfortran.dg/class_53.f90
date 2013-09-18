! { dg-do compile }
!
! PR 54778: [OOP] an ICE on invalid OO code
!
! Contributed by Sylwester Arabas <slayoo@staszic.waw.pl>

implicit none

type :: arr_t
  real :: at
end type

type(arr_t) :: this
class(arr_t) :: elem   ! { dg-error "must be dummy, allocatable or pointer" }

elem = this   ! { dg-error "Nonallocatable variable must not be polymorphic in intrinsic assignment" }

end
