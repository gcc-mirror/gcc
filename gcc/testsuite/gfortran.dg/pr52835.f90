! PR tree-optimization/52835
! { dg-do compile }
! { dg-options "-O3 -fdump-tree-optimized" }

subroutine foo (x, y, z, n)
  integer :: n, i
  real :: x(n), y(n), z(n)
  do i = 1, n
    z(i) = 0.0
    y(i) = 0.0
    call bar (y(i), z(i), x(i))
  end do
end subroutine

! { dg-final { scan-tree-dump "bar\[ _\]" "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
