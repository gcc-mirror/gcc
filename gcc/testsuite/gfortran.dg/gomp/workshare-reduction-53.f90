! { dg-do compile }
! { dg-options "-O2 -fopenmp -fdump-tree-optimized" }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ordered_start \[^\n\r]*, (?:2147483650|-2147483646), 4, " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_ordered_start " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_ordered_end " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ordered_dynamic_next " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } }

module m
  implicit none (type, external)
  integer :: j
  interface
    subroutine bar(i)
      integer :: i
    end subroutine
  end interface
end module m

subroutine foo(a, b, c)
  use m
  implicit none (type, external)
  integer :: a, b ,c
  integer :: i
  !$omp parallel
  !$omp do ordered reduction (task, *: j) schedule (dynamic, 4)
  do i = a, b, c
    call bar (j)
    !$omp ordered
    j = j + 1
    !$omp end ordered
  end do
  !$omp end parallel
end
