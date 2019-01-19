! { dg-options "-O3 -fdump-tree-lversion-details" }

subroutine f1(x)
  real :: x(:, :)
  x(1, :) = 100
end subroutine f1

subroutine f2(x, i)
  real :: x(:, :)
  integer :: i
  x(i, :) = 100
end subroutine f2

subroutine f3(x)
  real :: x(:, :)
  do j = lbound(x, 2), ubound(x, 2)
     x(1, j) = 100
  end do
end subroutine f3

subroutine f4(x, i)
  real :: x(:, :)
  integer :: i
  do j = lbound(x, 2), ubound(x, 2)
     x(i, j) = 100
  end do
end subroutine f4

! { dg-final { scan-tree-dump-times {likely to be the innermost dimension} 4 "lversion" } }
! { dg-final { scan-tree-dump-not {want to version} "lversion" } }
! { dg-final { scan-tree-dump-not {versioned} "lversion" } }
