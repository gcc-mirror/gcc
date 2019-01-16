! { dg-options "-O3 -fdump-tree-lversion-details" }

! Check that versioning can handle small groups of accesses, with the
! group being a separate array dimension.

subroutine f1(x, n, step)
  integer :: n, step
  real :: x(2, n * step)
  do i = 1, n
     x(1, i * step) = 100
     x(2, i * step) = 101
  end do
end subroutine f1

subroutine f2(x, limit, step)
  integer :: limit, step
  real :: x(2, limit)
  do i = 1, limit, step
     x(1, i) = 100
     x(2, i) = 101
  end do
end subroutine f2

subroutine f3(x, n, step)
  integer :: n, step
  real :: x(3, n * step)
  do i = 1, n
     x(1, i * step) = 100
     x(2, i * step) = 101
     x(3, i * step) = 102
  end do
end subroutine f3

subroutine f4(x, limit, step)
  integer :: limit, step
  real :: x(3, limit)
  do i = 1, limit, step
     x(1, i) = 100
     x(2, i) = 101
     x(3, i) = 102
  end do
end subroutine f4

subroutine f5(x, n, step)
  integer :: n, step
  real :: x(4, n * step)
  do i = 1, n
     x(1, i * step) = 100
     x(2, i * step) = 101
     x(3, i * step) = 102
     x(4, i * step) = 103
  end do
end subroutine f5

subroutine f6(x, limit, step)
  integer :: limit, step
  real :: x(4, limit)
  do i = 1, limit, step
     x(1, i) = 100
     x(2, i) = 101
     x(3, i) = 102
     x(4, i) = 103
  end do
end subroutine f6

! { dg-final { scan-tree-dump-times {want to version containing loop} 6 "lversion" } }
! { dg-final { scan-tree-dump-times {versioned this loop} 6 "lversion" } }
