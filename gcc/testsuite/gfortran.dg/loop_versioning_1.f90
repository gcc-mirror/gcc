! { dg-options "-O3 -fdump-tree-lversion-details" }

! The simplest IV case.

subroutine f1(x)
  real :: x(:)
  x(:) = 100
end subroutine f1

subroutine f2(x, n, step)
  integer :: n, step
  real :: x(n * step)
  do i = 1, n
     x(i * step) = 100
  end do
end subroutine f2

subroutine f3(x, limit, step)
  integer :: limit, step
  real :: x(limit)
  do i = 1, limit, step
     x(i) = 100
  end do
end subroutine f3

! { dg-final { scan-tree-dump-times {likely to be the innermost dimension} 1 "lversion" } }
! { dg-final { scan-tree-dump-times {want to version containing loop} 3 "lversion" } }
! { dg-final { scan-tree-dump-times {versioned this loop} 3 "lversion" } }
