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

subroutine common
  use m
  integer :: a,b,c(5)
  common /my/ a,b,c  ! { dg-error "Sorry, !.OMP allocate for COMMON block variable 'my' at .1. not supported" }
  !$omp allocate(/my/) allocator(omp_cgroup_mem_alloc)
end

integer function allocators() result(res)
  use m
  integer, save :: a(5) = [1,2,3,4,5]  ! { dg-error "Sorry, !.OMP allocate for variable 'a' at .1. with SAVE attribute not yet implemented" }
  !$omp allocate(a) allocator(omp_high_bw_mem_alloc)
  res = a(4)
end


