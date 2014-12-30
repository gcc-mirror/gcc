! { dg-do compile }
!
! PR 55037: [4.8 Regression] [OOP] ICE with local allocatable variable of abstract type
!
! Contributed by <mrestelli@gmail.com>

module m1
 implicit none
 type, abstract :: c_stv
 contains
  procedure, pass(x) :: source
 end type c_stv
contains
 subroutine source(y,x)
  class(c_stv), intent(in)               :: x
  class(c_stv), allocatable, intent(out) :: y
 end subroutine source
end module m1

module m2
 use m1, only : c_stv
 implicit none
contains
 subroutine sub(u0)
  class(c_stv), intent(inout) :: u0
  class(c_stv), allocatable :: tmp
   call u0%source(tmp)
 end subroutine sub
end module m2 


program p
 implicit none
 type :: c_stv
 end type
 class(c_stv), allocatable :: tmp
 call source(tmp)
contains
 subroutine source(y)
  type(c_stv), allocatable, intent(out) :: y
 end subroutine
end

! { dg-final { cleanup-modules "m1 m2" } }
