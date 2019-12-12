! { dg-options "-O3 -fdump-tree-lversion-details -fno-frontend-loop-interchange" }

! Make sure that in a "badly nested" loop, we don't treat the inner loop
! as iterating over the inner dimension with a variable stride.

subroutine f1(x, n)
  integer :: n
  real :: x(100, 100)
  do i = 1, n
     do j = 1, n
        x(i, j) = 100
     end do
  end do
end subroutine f1

subroutine f2(x, n, step)
  integer :: n, step
  real :: x(100, 100)
  do i = 1, n
     do j = 1, n
        x(i, j * step) = 100
     end do
  end do
end subroutine f2

subroutine f3(x, n)
  integer :: n
  real :: x(n, n)
  do i = 1, n
     do j = 1, n
        x(i, j) = 100
     end do
  end do
end subroutine f3

subroutine f4(x, n, step)
  integer :: n, step
  real :: x(n, n * step)
  do i = 1, n
     do j = 1, n
        x(i, j * step) = 100
     end do
  end do
end subroutine f4

subroutine f5(x, n, limit, step)
  integer :: n, limit, step
  real :: x(n, limit)
  do i = 1, n
     do j = 1, limit, step
        x(i, j) = 100
     end do
  end do
end subroutine f5

! { dg-final { scan-tree-dump-not {want to version} "lversion" } }
! { dg-final { scan-tree-dump-not {versioned} "lversion" } }
