! { dg-do run }
! Test the move_alloc intrinsic.
!
! Contributed by Erik Edelmann  <eedelmann@gcc.gnu.org>
!            and Paul Thomas  <pault@gcc.gnu.org>
!
program test_move_alloc

    implicit none
    integer, allocatable :: x(:), y(:), temp(:)
    character(4), allocatable :: a(:), b(:)
    integer :: i

    allocate (x(2))
    allocate (a(2))

    x = [ 42, 77 ]

    call move_alloc (x, y)
    if (allocated(x)) STOP 1
    if (.not.allocated(y)) STOP 2
    if (any(y /= [ 42, 77 ])) STOP 3

    a = [ "abcd", "efgh" ]
    call move_alloc (a, b)
    if (allocated(a)) STOP 4
    if (.not.allocated(b)) STOP 5
    if (any(b /= [ "abcd", "efgh" ])) STOP 6

    ! Now one of the intended applications of move_alloc; resizing

    call move_alloc (y, temp)
    allocate (y(6), stat=i)
    if (i /= 0) STOP 7
    y(1:2) = temp
    y(3:) = 99
    deallocate(temp)
    if (any(y /= [ 42, 77, 99, 99, 99, 99 ])) STOP 8
end program test_move_alloc
