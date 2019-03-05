! { dg-options "-O3 -fdump-tree-lversion-details -fno-frontend-loop-interchange" }

! Test cases in which versioning is useful for a two-dimensional array.

subroutine f1(x)
  real :: x(:, :)
  x(:, :) = 100
end subroutine f1

subroutine f2(x)
  real :: x(:, :)
  do i = lbound(x, 1), ubound(x, 1)
     do j = lbound(x, 2), ubound(x, 2)
        x(j, i) = 100
     end do
  end do
end subroutine f2

subroutine f3(x, n, step)
  integer :: n, step
  real :: x(100, 100)
  do i = 1, n
     do j = 1, n
        x(j * step, i) = 100
     end do
  end do
end subroutine f3

subroutine f4(x, n, step)
  integer :: n, step
  real :: x(n * step, n)
  do i = 1, n
     do j = 1, n
        x(j * step, i) = 100
     end do
  end do
end subroutine f4

subroutine f5(x, n, limit, step)
  integer :: n, limit, step
  real :: x(limit, n)
  do i = 1, n
     do j = 1, limit, step
        x(j, i) = 100
     end do
  end do
end subroutine f5

subroutine f6(x, y)
  real :: x(:, :), y(:)
  do i = lbound(x, 1), ubound(x, 1)
     do j = lbound(x, 2), ubound(x, 2)
        x(j, i) = 100
     end do
     y(i) = 200
  end do
end subroutine f6

subroutine f7(x, y, n, step)
  integer :: n, step
  real :: x(100, 100), y(100)
  do i = 1, n
     do j = 1, n
        x(j * step, i) = 100
     end do
     y(i * step) = 200
  end do
end subroutine f7

subroutine f8(x, y, n, step)
  integer :: n, step
  real :: x(n * step, n), y(n * step)
  do i = 1, n
     do j = 1, n
        x(j * step, i) = 100
     end do
     y(i * step) = 200
  end do
end subroutine f8

subroutine f9(x, n, limit, step)
  integer :: n, limit, step
  real :: x(limit, n), y(limit)
  do i = 1, n
     do j = 1, limit, step
        x(j, i) = 100
     end do
     y(i) = 200
  end do
end subroutine f9

! { dg-final { scan-tree-dump-times {likely to be the innermost dimension} 3 "lversion" } }
! { dg-final { scan-tree-dump-times {want to version containing loop} 9 "lversion" } }
! { dg-final { scan-tree-dump-times {hoisting check} 9 "lversion" } }
! { dg-final { scan-tree-dump-times {versioned this loop} 9 "lversion" } }
