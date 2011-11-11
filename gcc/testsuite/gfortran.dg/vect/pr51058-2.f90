! PR tree-optimization/51058
! { dg-do compile }
subroutine pr51058(n, u, v, w, z)
  double precision :: x(3,-2:16384), y(3,-2:16384), b, u, v, w, z
  integer :: i, n
  common /c/ x, y
  do i = 1, n
    b = u * int(x(1,i)) + sign(z,x(1,i))
    x(1,i) = x(1,i) - b
    y(1,i) = y(1,i) - b
    b = v * int(x(2,i)) + sign(z,x(2,i))
    x(2,i) = x(2,i) - b
    y(2,i) = y(2,i) - b
    b = w * int(x(3,i)) + sign(z,x(3,i))
    x(3,i) = x(3,i) - b
    y(3,i) = y(3,i) - b
  end do
end subroutine

! { dg-final { cleanup-tree-dump "vect" } }
