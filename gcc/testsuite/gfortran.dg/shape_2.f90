! Check that lbound() and ubound() work correctly for assumed shapes.
! { dg-do run }
program main
  integer, dimension (40, 80) :: a = 1
  call test (a)
contains
  subroutine test (b)
    integer, dimension (11:, -8:), target :: b
    integer, dimension (:, :), pointer :: ptr

    if (lbound (b, 1) .ne. 11) STOP 1
    if (ubound (b, 1) .ne. 50) STOP 2
    if (lbound (b, 2) .ne. -8) STOP 3
    if (ubound (b, 2) .ne. 71) STOP 4

    if (lbound (b (:, :), 1) .ne. 1) STOP 5
    if (ubound (b (:, :), 1) .ne. 40) STOP 6
    if (lbound (b (:, :), 2) .ne. 1) STOP 7
    if (ubound (b (:, :), 2) .ne. 80) STOP 8

    if (lbound (b (20:30:3, 40), 1) .ne. 1) STOP 9
    if (ubound (b (20:30:3, 40), 1) .ne. 4) STOP 10

    ptr => b
    if (lbound (ptr, 1) .ne. 11) STOP 11
    if (ubound (ptr, 1) .ne. 50) STOP 12
    if (lbound (ptr, 2) .ne. -8) STOP 13
    if (ubound (ptr, 2) .ne. 71) STOP 14
  end subroutine test
end program main
