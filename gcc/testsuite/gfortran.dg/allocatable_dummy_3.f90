! { dg-do run }
! PR 28416: Check that allocatable dummies can be passed onwards as non-assumed
! shape arg.
program main

    implicit none
    integer, allocatable :: a(:)

    interface
        subroutine foo(v_out)
            integer, allocatable :: v_out(:)
        end subroutine foo
    end interface

    call foo(a)
    if (any(a /= [ 1, 2, 3 ])) STOP 1

end program


subroutine foo(v_out)
    implicit none
    integer, allocatable :: v_out(:)

    allocate(v_out(3))
    call bar(v_out, size(v_out))
end subroutine foo


subroutine bar(v, N)
    implicit none
    integer :: N
    integer :: v(N)
    integer :: i

    do i = 1, N
        v(i) = i
    end do
end subroutine bar
