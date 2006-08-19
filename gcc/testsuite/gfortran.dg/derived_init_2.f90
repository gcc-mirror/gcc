! { dg-do run }
! PR 25217: INTENT(OUT) dummies of derived type with default initializers shall
! be (re)initialized upon procedure entry, unless they are ALLOCATABLE.
program main

    implicit none

    type :: drv
        integer :: a(3) = [ 1, 2, 3 ]
        character(3) :: s = "abc"
        real, pointer :: p => null()
    end type drv
    type(drv) :: aa
    type(drv), allocatable :: ab(:)
    real, target :: x

    aa%a = [ 4, 5, 6]
    aa%s = "def"
    aa%p => x
    call sub(aa)

contains

    subroutine sub(fa)
        type(drv), intent(out) :: fa

        if (any(fa%a /= [ 1, 2, 3 ])) call abort()
        if (fa%s /= "abc") call abort()
        if (associated(fa%p)) call abort()
    end subroutine sub

end program main
