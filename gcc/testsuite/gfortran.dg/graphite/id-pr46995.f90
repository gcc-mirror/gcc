! { dg-options "-O -ffast-math -fgraphite-identity -fno-tree-dce" }

subroutine foo (m, l, zw)
  integer :: m, i, j, k
  real, dimension(1:9) :: zw
  real :: l, s
  s = 0
  do i = 1, 9
    do j = 1, 2*m
      do k = 1, 2*m
        s = s + 1
      end do
    end do
    l = l + zw(i)*s
  end do
end subroutine foo
