! { dg-options "-O3 -fdump-tree-lversion-details -fno-frontend-loop-interchange" }

! We could version the loop for when the first dimension has a stride
! of 1, but at present there's no real benefit.  The gimple loop
! interchange pass couldn't handle the versioned loop, and interchange
! is instead done by the frontend (but disabled by the options above).

subroutine f1(x)
  real :: x(:, :)
  do i = lbound(x, 1), ubound(x, 1)
     do j = lbound(x, 2), ubound(x, 2)
        x(i, j) = 100
     end do
  end do
end subroutine f1

subroutine f2(x, n, step)
  integer :: n, step
  real :: x(100, 100)
  do i = 1, n
     do j = 1, n
        x(i * step, j) = 100
     end do
  end do
end subroutine f2

subroutine f3(x, n, step)
  integer :: n, step
  real :: x(n * step, n)
  do i = 1, n
     do j = 1, n
        x(i * step, j) = 100
     end do
  end do
end subroutine f3

! { dg-final { scan-tree-dump-times {likely to be the innermost dimension} 1 "lversion" } }
! { dg-final { scan-tree-dump-not {want to version} "lversion" } }
! { dg-final { scan-tree-dump-not {versioned} "lversion" } }
