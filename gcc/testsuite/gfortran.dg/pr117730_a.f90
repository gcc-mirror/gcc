! { dg-do compile }
!
! Test the fix for PR117730 in which the non_overrridable procedures in 'child'
! were mixied up in the vtable for the extension 'child2' in pr117730_b.f90.
! This resulted in 'this%calc()' in 'function child_get(this)' returning garbage
! when 'this' was of dynamic type 'child2'.
!
! Contributed by  <daraja@web.de> in comment 4 of PR84674.
!
module module1
    implicit none
    private
    public :: child

    type, abstract :: parent
    contains
        procedure, pass :: reset => parent_reset
    end type parent

    type, extends(parent), abstract :: child
    contains
        procedure, pass, non_overridable :: reset => child_reset
        procedure, pass, non_overridable :: get => child_get
        procedure(calc_i), pass, deferred :: calc
    end type child

    abstract interface
        pure function calc_i(this) result(value)
            import :: child
            class(child), intent(in) :: this
            integer                 :: value
        end function calc_i
    end interface

contains
    pure subroutine parent_reset(this)
        class(parent), intent(inout) :: this
    end subroutine parent_reset

    pure subroutine child_reset(this)
        class(child), intent(inout) :: this
    end subroutine child_reset

    function child_get(this) result(value)
        class(child), intent(inout) :: this
        integer                   :: value

        value = this%calc()
    end function child_get
end module module1
