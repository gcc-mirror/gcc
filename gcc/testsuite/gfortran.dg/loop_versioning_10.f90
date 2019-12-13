! { dg-options "-O3 -fdump-tree-lversion-details" }

subroutine f1(x)
  real :: x(:, :)
  x(:, 1) = 100
end subroutine f1

subroutine f2(x, i)
  real :: x(:, :)
  integer :: i
  x(:, i) = 100
end subroutine f2

subroutine f3(x)
  real :: x(:, :)
  do j = lbound(x, 1), ubound(x, 1)
     x(j, 1) = 100
  end do
end subroutine f3

subroutine f4(x, i)
  real :: x(:, :)
  integer :: i
  do j = lbound(x, 1), ubound(x, 1)
     x(j, i) = 100
  end do
end subroutine f4

! { dg-final { scan-tree-dump-times {likely to be the innermost dimension} 4 "lversion" } }
! { dg-final { scan-tree-dump-times {want to version} 4 "lversion" } }
! { dg-final { scan-tree-dump-times {versioned} 4 "lversion" } }
