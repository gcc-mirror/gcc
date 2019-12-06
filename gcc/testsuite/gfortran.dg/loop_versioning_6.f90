! { dg-options "-O3 -fdump-tree-lversion-details" }

! Check that versioning can handle small groups of accesses.

subroutine f1(x)
  real :: x(:)
  do i = lbound(x, 1), ubound(x, 1) / 2
     x(i * 2) = 100
     x(i * 2 + 1) = 101
  end do
end subroutine f1

subroutine f2(x, n, step)
  integer :: n, step
  real :: x(n * step * 2)
  do i = 1, n
     x(i * step * 2) = 100
     x(i * step * 2 + 1) = 101
  end do
end subroutine f2

subroutine f3(x, limit, step)
  integer :: limit, step
  real :: x(limit * 2)
  do i = 1, limit, step
     x(i * 2) = 100
     x(i * 2 + 1) = 101
  end do
end subroutine f3

subroutine f4(x)
  real :: x(:)
  do i = lbound(x, 1), ubound(x, 1) / 3
     x(i * 3) = 100
     x(i * 3 + 1) = 101
     x(i * 3 + 2) = 102
  end do
end subroutine f4

subroutine f5(x, n, step)
  integer :: n, step
  real :: x(n * step * 3)
  do i = 1, n
     x(i * step * 3) = 100
     x(i * step * 3 + 1) = 101
     x(i * step * 3 + 2) = 102
  end do
end subroutine f5

subroutine f6(x, limit, step)
  integer :: limit, step
  real :: x(limit * 3)
  do i = 1, limit, step
     x(i * 3) = 100
     x(i * 3 + 1) = 101
     x(i * 3 + 2) = 102
  end do
end subroutine f6

subroutine f7(x)
  real :: x(:)
  do i = lbound(x, 1), ubound(x, 1) / 4
     x(i * 4) = 100
     x(i * 4 + 1) = 101
     x(i * 4 + 2) = 102
     x(i * 4 + 3) = 103
  end do
end subroutine f7

subroutine f8(x, n, step)
  integer :: n, step
  real :: x(n * step * 4)
  do i = 1, n
     x(i * step * 4) = 100
     x(i * step * 4 + 1) = 101
     x(i * step * 4 + 2) = 102
     x(i * step * 4 + 3) = 103
  end do
end subroutine f8

subroutine f9(x, limit, step)
  integer :: limit, step
  real :: x(limit * 4)
  do i = 1, limit, step
     x(i * 4) = 100
     x(i * 4 + 1) = 101
     x(i * 4 + 2) = 102
     x(i * 4 + 3) = 103
  end do
end subroutine f9

! { dg-final { scan-tree-dump-times {want to version containing loop} 9 "lversion" { xfail { ! lp64 } } } }
! { dg-final { scan-tree-dump-times {versioned this loop} 9 "lversion" { xfail { ! lp64 } } } }
