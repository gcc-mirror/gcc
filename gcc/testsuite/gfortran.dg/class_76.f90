! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/90069
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
!

program returned_memory_leak
    implicit none

    type, abstract :: base
    end type base

    type, extends(base) :: extended
    end type extended

    type :: container
        class(*), allocatable :: thing
    end type

    call run()
contains
    subroutine run()
        type(container) :: a_container

        a_container = theRightWay()
        a_container = theWrongWay()
    end subroutine

    function theRightWay()
        type(container) :: theRightWay

        class(base), allocatable :: thing

        allocate(thing, source = newAbstract())
        theRightWay = newContainer(thing)
    end function theRightWay

    function theWrongWay()
        type(container) :: theWrongWay

        theWrongWay = newContainer(newAbstract())
    end function theWrongWay

    function  newAbstract()
        class(base), allocatable :: newAbstract

        allocate(newAbstract, source = newExtended())
    end function newAbstract

    function newExtended()
        type(extended) :: newExtended
    end function newExtended

    function newContainer(thing)
        class(*), intent(in) :: thing
        type(container) :: newContainer

        allocate(newContainer%thing, source = thing)
    end function newContainer
end program returned_memory_leak

! { dg-final { scan-tree-dump-times "newabstract" 15 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free" 8 "original" } }

