! { dg-do compile }
!
! Tests the fixes for PR82943.
!
! Contributed by Alexander Westbrooks  <ctechnodev@gmail.com>
!
module m
    public :: foo, bar, foobar

    type, public :: good_type(n)
       integer, len :: n = 1
    contains
       procedure :: foo
    end type

    type, public :: good_type2(k)
       integer, kind :: k = 1                                 
    contains
       procedure :: bar
    end type

    type, public :: good_type3(n, k)
        integer, len :: n = 1
       integer, kind :: k = 1
    contains
       procedure :: foobar
    end type
  
    contains
        subroutine foo(this)
            class(good_type(*)), intent(inout) :: this
        end subroutine

        subroutine bar(this)
            class(good_type2(2)), intent(inout) :: this
        end subroutine

        subroutine foobar(this)
            class(good_type3(*,2)), intent(inout) :: this
        end subroutine

 end module