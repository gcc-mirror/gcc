! { dg-do  run }
! { dg-options "-ffrontend-optimize" }
! PR 66111 - this used to ICE with matmul inlining.
! Original test case by Mikael Morin.

implicit none
  integer, parameter :: n = 4
  integer, dimension(n, n) :: a, b, c
  integer, dimension(n*n)  :: p, res, res2
  integer, dimension(n)    :: v

  integer :: i

  p = [ +59, -53, +47, -43, &
        -37, +31, -29, +23, &
        +19, -17, +13, -11, &
        - 7, + 5, - 3, + 2  ]
  a = reshape(p, shape(a))
  b = reshape([(i, i=1, size(a))], shape(b))
  v = [ 3, 1, 2, 4]
  c = matmul(a, b)
  res = [ + 14, - 22, + 16, - 22, &
          +150, -158, +128, -138, &
          +286, -294, +240, -254, &
          +422, -430, +352, -370  ]
  !print *,c
  if (any(c /= reshape(res, shape(c)))) call abort
  c(:,v) = matmul(a, b)
  if (any(c(:,v) /= reshape(res, shape(c)))) call abort
  c(v,:) = matmul(a, b)
  if (any(c(v,:) /= reshape(res, shape(c)))) call abort

  c = matmul(a(:,v),b(v,:))
  if (any(c /= reshape(res, shape(c)))) call abort
end
