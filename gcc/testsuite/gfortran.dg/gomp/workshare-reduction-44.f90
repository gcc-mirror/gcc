! { dg-do compile }
! { dg-options "-O2 -fopenmp -fdump-tree-optimized" }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop(?:_ull)?_start \[^\n\r]*, 3, 1, " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop(?:_ull)?_nonmonotonic_guided_next " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } }

module m
  implicit none (type, external)
  integer(8) :: j
  interface
    subroutine bar(i)
      integer(8) :: i
    end subroutine
  end interface
end module m

subroutine foo(a, b, c)
  use m
  implicit none (type, external)
  integer(8) :: a, b ,c
  integer(8) :: i
  !$omp parallel
  !$omp do reduction (task, *: j) schedule (nonmonotonic: guided)
  do i = a, b, c
    j = j + 1
    call bar (j)
  end do
  !$omp end parallel
end
