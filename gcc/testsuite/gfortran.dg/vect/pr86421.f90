! PR fortran/86421
! { dg-require-effective-target vect_simd_clones }
! { dg-additional-options "-fopenmp-simd" }
! { dg-additional-options "-mavx" { target avx_runtime } }

module mod86421
  implicit none
contains
  subroutine foo(x, y, z)
    real :: x
    integer :: y, z
    !$omp declare simd linear(ref(x)) linear(val(y)) linear(uval(z))
    x = x + y
    z = z + 1
  end subroutine
end module mod86421

program pr86421
  use mod86421
  implicit none
  integer :: i, j
  real :: a(64)
  j = 0
  do i = 1, 64
    a(i) = i
  end do
  !$omp simd
  do i = 1, 64
    call foo (a(i), i, j)
  end do
  do i = 1, 64
    if (a(i) .ne. (2 * i)) stop 1
  end do
  if (j .ne. 64) stop 2
end program pr86421
