! PR fortran/87752
! { dg-do compile }
! { dg-additional-options "-Ofast" }

subroutine foo (n, u, v)
  integer :: n
  real, pointer :: u(:), v(:)
  !$omp parallel do simd
  do i = 1, n
    u(:) = v(:)
  end do
end
