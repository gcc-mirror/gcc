! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/90068
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
! 

program array_memory_leak
    implicit none

    type, abstract :: base
    end type base

    type, extends(base) :: extended
    end type extended

    type :: container
        class(base), allocatable :: thing
    end type

    type, extends(base) :: collection
        type(container), allocatable :: stuff(:)
    end type collection

    call run()
    call bad()
contains
    subroutine run()
        type(collection) :: my_thing
        type(container) :: a_container

        a_container = newContainer(newExtended()) ! This is fine
        my_thing = newCollection([a_container])
    end subroutine run

    subroutine bad()
        type(collection) :: my_thing

        my_thing = newCollection([newContainer(newExtended())]) ! This is a memory leak
    end subroutine bad

    function newExtended()
        type(extended) :: newExtended
    end function newExtended

    function newContainer(thing)
        class(base), intent(in) :: thing
        type(container) :: newContainer

        allocate(newContainer%thing, source = thing)
    end function newContainer

    function newCollection(things)
        type(container), intent(in) :: things(:)
        type(collection) :: newCollection

        newCollection%stuff = things
    end function newCollection
end program array_memory_leak

! { dg-final { scan-tree-dump-times "__builtin_free" 15 "original" } }

