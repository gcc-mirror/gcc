! { dg-do compile }

module m
  implicit none (type, external)
contains
  real function add(x, y, j) result(res)
    !$omp declare simd(add) uniform(x, y) linear(j : 1) simdlen(4)
    integer, value :: j
    real, intent(in) :: x(*), y(*)
    res = x(j) + y(j)
  end function
end module m

program main
  use m
  implicit none (type, external)
  real, allocatable :: A(:), B(:), C(:)
  integer :: i, N
  N = 128
  A = [(3*i, i = 1, N)]
  B = [(7*i, i = 1, N)]
  allocate (C(N))

  !$omp simd
  do i = 1, N
    C(i) = add(A, B, i)
  end do

  if (any (C /= [(10*i, i = 1, N)])) error stop
end program main
