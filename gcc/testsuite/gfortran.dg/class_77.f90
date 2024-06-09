! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/90072
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
! 

module types
    implicit none

    type, abstract :: base_returned
    end type base_returned

    type, extends(base_returned) :: first_returned
    end type first_returned

    type, extends(base_returned) :: second_returned
    end type second_returned

    type, abstract :: base_called
    contains
        procedure(get_), deferred :: get
    end type base_called

    type, extends(base_called) :: first_extended
    contains
        procedure :: get => getFirst
    end type first_extended

    type, extends(base_called) :: second_extended
    contains
        procedure :: get => getSecond
    end type second_extended

    abstract interface
        function get_(self) result(returned)
            import base_called
            import base_returned
            class(base_called), intent(in) :: self
            class(base_returned), allocatable :: returned
        end function get_
    end interface
contains
    function getFirst(self) result(returned)
        class(first_extended), intent(in) :: self
        class(base_returned), allocatable :: returned

        allocate(returned, source = first_returned())
    end function getFirst

    function getSecond(self) result(returned)
        class(second_extended), intent(in) :: self
        class(base_returned), allocatable :: returned

        allocate(returned, source = second_returned())
    end function getSecond
end module types

program dispatch_memory_leak
    implicit none

    call run()
contains
    subroutine run()
        use types, only: base_returned, base_called, first_extended

        class(base_called), allocatable :: to_call
        class(base_returned), allocatable :: to_get

        allocate(to_call, source = first_extended())
        allocate(to_get, source = to_call%get())

        deallocate(to_get)
        select type(to_call)
        type is (first_extended)
            allocate(to_get, source = to_call%get())
        end select
    end subroutine run
end program dispatch_memory_leak

! { dg-final { scan-tree-dump-times "__builtin_free" 5 "original" } }

