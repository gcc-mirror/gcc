! Check that lbound() and ubound() work correctly for assumed shapes.
! { dg-do run }
program main
  integer, dimension (40, 80) :: a = 1
  call test (a)
contains
  subroutine test (b)
    integer, dimension (11:, -8:), target :: b
    integer, dimension (:, :), pointer :: ptr

    if (lbound (b, 1) .ne. 11) call abort
    if (ubound (b, 1) .ne. 50) call abort
    if (lbound (b, 2) .ne. -8) call abort
    if (ubound (b, 2) .ne. 71) call abort

    if (lbound (b (:, :), 1) .ne. 1) call abort
    if (ubound (b (:, :), 1) .ne. 40) call abort
    if (lbound (b (:, :), 2) .ne. 1) call abort
    if (ubound (b (:, :), 2) .ne. 80) call abort

    if (lbound (b (20:30:3, 40), 1) .ne. 1) call abort
    if (ubound (b (20:30:3, 40), 1) .ne. 4) call abort

    ptr => b
    if (lbound (ptr, 1) .ne. 1) call abort
    if (ubound (ptr, 1) .ne. 40) call abort
    if (lbound (ptr, 2) .ne. 1) call abort
    if (ubound (ptr, 2) .ne. 80) call abort
  end subroutine test
end program main
