! { dg-do compile }
!
! Test the fix for PR98472.
!
! Contributed by Rui Coelho  <ruicoelhopedro@hotmail.com>
!
module a
        type, abstract :: base
        contains
                procedure(elem_func), deferred, nopass :: add
        end type base

        type, extends(base) :: derived
        contains
                procedure, nopass :: add => add_derived
        end type derived

        abstract interface
                elemental function elem_func(x, y) result(out)
                        integer, intent(in) :: x, y
                        integer :: out
                end function elem_func
        end interface

contains
        elemental function add_derived(x, y) result(out)
                integer, intent(in) :: x, y
                integer :: out
                out = x + y
        end function add_derived
end module a

program main
        use a
        call foo
contains
        subroutine foo
               integer, dimension(:), allocatable :: vec
               class(base), allocatable :: instance
               allocate(derived :: instance)
               allocate(vec, source=instance%add([1, 2], [1, 2])) ! ICE here
               if (any (vec .ne. [2, 4])) stop 1
        end
end program main


