! { dg-do compile }
!
! PR 46344: [4.6 Regression] [OOP] ICE with allocatable CLASS components
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

module m

  type t1
  end type

  type  t2
    class(t1), allocatable :: cc
  end type

  class(t2), allocatable :: sm

end module m


module m2

  type t1
  end type

  type  t2
    class(t1), allocatable :: c
  end type

  type(t1) :: w

end module m2


program p
  use m
  implicit none

  type(t2), allocatable :: x(:) 

  allocate(x(1))

end program p

! { dg-final { cleanup-modules "m m2" } }
