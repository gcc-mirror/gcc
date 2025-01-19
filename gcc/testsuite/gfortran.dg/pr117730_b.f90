! { dg-do run }
! { dg-compile-aux-modules "pr117730_a.f90" }
! { dg-additional-sources pr117730_a.f90 }
!
! Test the fix for PR117730 in which the non_overrridable procedures in
! pr117730_a.f90 were mixied up in the vtable for 'child2' below. This resulted
! in 'this%calc()' in 'function child_get(this)' returning garbage.
!
! Contributed by  <daraja@web.de> in comment 4 of PR84674.
!
module module2
    use module1, only: child

    implicit none
    private
    public :: child2

    type, extends(child) :: child2
    contains
        procedure, pass :: calc => child2_calc
    end type child2

contains

    pure function child2_calc(this) result(value)
        class(child2), intent(in) :: this
        integer :: value

        value = 1
    end function child2_calc

end module module2

program test
    use module2, only: child2

    implicit none

    type(child2) :: F

    if (F%calc() /= 1) stop 1

    print *, "---------------"
    if (F%get() /= 1) stop 2

end program test
! { dg-final { cleanup-modules "module1" } }
