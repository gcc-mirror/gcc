! { dg-do compile }
! { dg-options "-O2 -fopenmp -fdump-tree-optimized" }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_start \[^\n\r]*, (?:2147483649|-2147483647), 0, 0B, 0B, " 1 "optimized" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } }
! { dg-final { scan-tree-dump-not "__builtin_GOMP_loop\[^\n\r]*_next " "optimized" } }
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
  !$omp do reduction (task, *: j)
  do i = a, b, c
    j = j + 1
    call bar (j)
  end do
  !$omp end parallel
end
