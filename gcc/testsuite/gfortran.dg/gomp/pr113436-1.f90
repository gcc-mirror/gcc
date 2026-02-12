! PR middle-end/113436
! { dg-do compile }
! { dg-options "-fopenmp -fdump-tree-omplower" }

program g
  use omp_lib
  implicit none

  integer :: A, B(10)
  integer, allocatable :: C(:)
  integer :: i

  A = 5;
  allocate(C(10))
  do i = 1, 10
    B(i) = i + 5
    C(i) = B(i)
  end do
      
  !$omp target private(A) private(B) private(C) allocate(allocator(omp_high_bw_mem_alloc), align(64): A, B, C)
      A = 99
      do i = 1, 10
        B(i) = -i - 23
        C(i) = i + 23
      end do
  !$omp end target
end program g

! { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, 4, 4\\\);" "omplower" { target int32 } } }
! { dg-final { scan-tree-dump "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, 40, 4\\\);" "omplower" { target int32 } } }
! { dg-final { scan-tree-dump-times "D\\\.\[0-9\]\+ = __builtin_GOMP_alloc \\\(64, \[0-9\]\+, 4\\\);" 3 "omplower" } }
! { dg-final { scan-tree-dump "\\\*D\\\.\[0-9\]\+ = 99;" "omplower" } }
! { dg-final { scan-tree-dump "\\\(\\\*D\\\.\[0-9\]\+\\\)\\\[D\\\.\[0-9\]\+\\\] = D\\\.\[0-9\]\+;" "omplower" } }
! { dg-final { scan-tree-dump "D\\\.\[0-9\]\+\\\]\\\[D\\\.\[0-9\]\+\\\] = D\\\.\[0-9\]\+;" "omplower" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\\(D\\\.\[0-9\]+, 4\\\)" 3 "omplower" } }
