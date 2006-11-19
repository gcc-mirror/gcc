! { dg-do compile }
! Check that ALLOCATABLE components aren't allowed to the right of a non-zero
! rank part reference.
program test

    implicit none
    type :: foo
        real, allocatable :: bar(:)
    end type foo
    type(foo), target :: x(3)
    integer :: i
    real, pointer :: p(:)

    allocate(x(:)%bar(5))! { dg-error "must not have the ALLOCATABLE attribute" }
    x(:)%bar(1) = 1.0    ! { dg-error "must not have the ALLOCATABLE attribute" }
    p => x(:)%bar(1)     ! { dg-error "must not have the ALLOCATABLE attribute" }

end program test
