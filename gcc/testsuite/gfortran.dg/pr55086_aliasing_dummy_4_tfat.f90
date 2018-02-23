! { dg-do run }
! { dg-options "-ftest-forall-temp" }
! This is a copy of aliasing_dummy_4.f90, with an option set to improve
! test coverage by forcing forall code to use a temporary.
!
program  test_f90

    integer, parameter :: N = 2

    type test_type
        integer a(N, N)
    end type

    type (test_type) s(N, N)

    forall (l = 1:N, m = 1:N) &
        s(l, m)%a(:, :) = reshape ([((i*l + 10*j*m +100, i = 1, N), j = 1, N)], [N, N])

    call test_sub(s%a(1, 1), 1000) ! Test the original problem.

    if ( any (s(1, 1)%a(:, :) /= reshape ([1111, 112, 121, 122], [2, 2]))) STOP 1
    if ( any (s(1, 2)%a(:, :) /= reshape ([1121, 122, 141, 142], [2, 2]))) STOP 2
    if ( any (s(2, 1)%a(:, :) /= reshape ([1112, 114, 122, 124], [2, 2]))) STOP 3
    if ( any (s(2, 2)%a(:, :) /= reshape ([1122, 124, 142, 144], [2, 2]))) STOP 4

    call test_sub(s(1, 1)%a(:, :), 1000)  ! Check "normal" references.

    if ( any (s(1, 1)%a(:, :) /= reshape ([2111,1112,1121,1122], [2, 2]))) STOP 5
    if ( any (s(1, 2)%a(:, :) /= reshape ([1121, 122, 141, 142], [2, 2]))) STOP 6
    if ( any (s(2, 1)%a(:, :) /= reshape ([1112, 114, 122, 124], [2, 2]))) STOP 7
    if ( any (s(2, 2)%a(:, :) /= reshape ([1122, 124, 142, 144], [2, 2]))) STOP 8
contains
  subroutine test_sub(array, offset)
    integer array(:, :), offset

    forall (i = 1:N, j = 1:N) &
        array(i, j) = array(i, j) + offset
  end subroutine
end program

