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
    if (allocated(x)) call abort()
    if (.not.allocated(y)) call abort()
    if (any(y /= [ 42, 77 ])) call abort()

    a = [ "abcd", "efgh" ]
    call move_alloc (a, b)
    if (allocated(a)) call abort()
    if (.not.allocated(b)) call abort()
    if (any(b /= [ "abcd", "efgh" ])) call abort()

    ! Now one of the intended applications of move_alloc; resizing

    call move_alloc (y, temp)
    allocate (y(6), stat=i)
    if (i /= 0) call abort()
    y(1:2) = temp
    y(3:) = 99
    deallocate(temp)
    if (any(y /= [ 42, 77, 99, 99, 99, 99 ])) call abort()
end program test_move_alloc
