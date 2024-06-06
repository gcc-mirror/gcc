! { dg-do run }
!
! PR fortran/90076
!
! Contributed by Brad Richardson  <everythingfunctional@protonmail.com>
! 

program assignment_memory_leak
    implicit none

    type, abstract :: base
    end type base

    type, extends(base) :: extended
    end type extended

    call run()
contains
    subroutine run()
        class(base), allocatable :: var

        var = newVar() ! Crash fixed
    end subroutine run

    function newVar()
        class(extended), allocatable :: newVar
    end function newVar
end program assignment_memory_leak

