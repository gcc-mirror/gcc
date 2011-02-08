! { dg-options "-O -ffast-math -fgraphite-identity -fno-tree-dce" }

subroutine foo (m)
  integer :: m, i, j, k
  real :: s
  s = 0
  do i = 1, 9
    do j = 1, 2*m
      do k = 1, 2*m
        s = s + 1
      end do
    end do
  end do
end subroutine foo
