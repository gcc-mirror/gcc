! { dg-options "-fno-openmp -fopenmp-simd" }

subroutine test15
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll partial(1)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test15

subroutine test16
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll partial(2)
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test16

subroutine test19
  implicit none
  integer :: i

  !$omp simd
  !$omp unroll partial
  do i = 1,100
    call dummy(i)
  end do
  !$omp end unroll
end subroutine test19
