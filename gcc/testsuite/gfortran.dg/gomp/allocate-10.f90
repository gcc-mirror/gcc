! { dg-additional-options "-Wall -fdump-tree-gimple" }

module m
use iso_c_binding
integer, parameter :: omp_allocator_handle_kind = c_intptr_t
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_null_allocator = 0
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_default_mem_alloc = 1
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_large_cap_mem_alloc = 2
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_const_mem_alloc = 3
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_high_bw_mem_alloc = 4
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_low_lat_mem_alloc = 5
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_cgroup_mem_alloc = 6
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_pteam_mem_alloc = 7
        integer (kind=omp_allocator_handle_kind), &
                 parameter :: omp_thread_mem_alloc = 8
end


! { dg-final { scan-tree-dump-times "__builtin_GOMP_alloc" 3 "gimple" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free" 3 "gimple" } }

subroutine f
  use m
  implicit none
  integer :: n
  block
    integer :: A(n) ! { dg-warning "Unused variable 'a' declared" }
  end block
end

subroutine f2
  use m
  implicit none
  integer :: n  ! { dg-note "'n' was declared here" }
  block
    integer :: A(n)  ! { dg-warning "'n' is used uninitialized" }
    !$omp allocate(A)
    ! by matching 'A' above, TREE_USE is set. Hence:
    ! { dg-final { scan-tree-dump-times "a = __builtin_GOMP_alloc \\(., D\.\[0-9\]+, 0B\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(a, 0B\\);" 1 "gimple" } }
  end block
end

subroutine h1()
  use m
  implicit none
  integer(omp_allocator_handle_kind) my_handle  ! { dg-note "'my_handle' was declared here" }
  integer :: B1(3)
  !$omp allocate(B1) allocator(my_handle)  ! { dg-warning "31:'my_handle' is used uninitialized" }
  B1(1) = 5
  ! { dg-final { scan-tree-dump-times "b1.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 12, D\.\[0-9\]+\\);" 1 "gimple" } }
  ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(b1.\[0-9\]+, 0B\\);" 1 "gimple" } }
end

subroutine h2()
  use m
  implicit none
  integer(omp_allocator_handle_kind) my_handle  ! { dg-note "'my_handle' was declared here" }
  block
    integer :: B2(3)
    !$omp allocate(B2) allocator(my_handle)  ! { dg-warning "33:'my_handle' is used uninitialized" }
    ! Similar above; B2 is unused - but in gfortran, the match in 'allocate(B2)' already
    ! causes TREE_USED = 1
    ! { dg-final { scan-tree-dump-times "b2.\[0-9\]+ = __builtin_GOMP_alloc \\(4, 12, D\.\[0-9\]+\\);" 1 "gimple" } }
    ! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(b2.\[0-9\]+, 0B\\);" 1 "gimple" } }
  end block
end
