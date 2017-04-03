! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo (v, n, r)
  integer :: n
  integer, intent(in) :: v(:)
  integer, intent(out) :: r
  integer :: i

  r = 0

!$omp parallel
!$omp single

  do i = 1, n
!$omp task shared (v)
    r = r + v(i)
!$omp end task
  enddo

!$omp end single
!$omp end parallel

end
