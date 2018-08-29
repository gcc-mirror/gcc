! { dg-do run }
! Check that allocatable/pointer variables of derived types with initialized
! components are are initialized when allocated
! PR 21625
program test

    implicit none
    type :: t
        integer :: a = 3
    end type t
    type :: s
        type(t), pointer :: p(:)
        type(t), pointer :: p2
    end type s
    type(t), pointer :: p
    type(t), allocatable :: q(:,:)
    type(s) :: z
    type(s) :: x(2)

    allocate(p, q(2,2))
    if (p%a /= 3) STOP 1
    if (any(q(:,:)%a /= 3)) STOP 2

    allocate(z%p2, z%p(2:3))
    if (z%p2%a /= 3) STOP 3
    if (any(z%p(:)%a /= 3)) STOP 4

    allocate(x(1)%p2, x(1)%p(2))
    if (x(1)%p2%a /= 3) STOP 5
    if (any(x(1)%p(:)%a /= 3)) STOP 6
end program test

