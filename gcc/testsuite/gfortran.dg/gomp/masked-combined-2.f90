subroutine foo (a)
  implicit none (external, type)
  integer, contiguous :: a(0:)
  integer :: i, r, s
  r = 0; s = 0

  ! In 'parallel masked taskloop', in_reduction is not permitted.

  !$omp taskgroup task_reduction(+:r)
    !$omp parallel masked taskloop in_reduction(+:r)  ! { dg-error "36: Failed to match clause" }
      do i = 0, 63
        r = r + a(i)
      end do
    !!$omp end parallel masked taskloop
  !$omp end taskgroup

  !$omp taskgroup task_reduction(+:s)
    !$omp parallel masked taskloop simd in_reduction(+:s)  ! { dg-error "41: Failed to match clause" }
      do i = 0, 63
        s = s + a(i)
      end do
    !!$omp end parallel masked taskloop simd
  !$omp end taskgroup
end
