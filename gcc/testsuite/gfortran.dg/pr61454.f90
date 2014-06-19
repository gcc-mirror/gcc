! { dg-do compile }

  implicit none
  integer, parameter :: arr(2) = [ 1, 3 ]
  real, parameter :: arr2(2) = [ 1.5, 2.1 ]
  integer, parameter :: j = int(sum(arr))
  integer, parameter :: k = ceiling(sum(arr2))
  real(kind=j) :: x1
  real(kind=k) :: x2

  print *, j, k
  print *, x1, x2

  end
