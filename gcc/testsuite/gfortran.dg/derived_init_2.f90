! { dg-do run }
! PR 25217: INTENT(OUT) dummies of derived type with default initializers shall
! be (re)initialized upon procedure entry, unless they are ALLOCATABLE.
! Modified to take account of the regression, identified by Martin Tees
! http://gcc.gnu.org/ml/fortran/2006-08/msg00276.html and fixed with
! PR 28788.
module dt
    type :: drv
        integer :: a(3) = [ 1, 2, 3 ]
        character(3) :: s = "abc"
        real, pointer :: p => null()
    end type drv
end module dt

module subs
contains
    subroutine foo(fb)
        use dt
        type(drv), intent(out) :: fb
        call sub (fb)
    end subroutine foo

    subroutine sub(fa)
        use dt
        type(drv), intent(out) :: fa

        if (any(fa%a /= [ 1, 2, 3 ])) call abort()
        if (fa%s /= "abc") call abort()
        if (associated(fa%p)) call abort()
    end subroutine sub
end module subs

program main
    use dt
    use subs
    implicit none
    type(drv) :: aa
    type(drv), allocatable :: ab(:)
    real, target :: x = 99, y = 999

    aa = drv ([ 4, 5, 6], "def", x)
    call sub(aa)

    aa = drv ([ 7, 8, 9], "ghi", y)
    call foo(aa)
end program main

