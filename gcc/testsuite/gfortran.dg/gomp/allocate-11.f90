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

subroutine f ()
  use m
  implicit none
  integer :: i
  !$omp parallel firstprivate(i) allocate(allocator(omp_low_latency_mem_alloc): i)
    ! { dg-error "Symbol 'omp_low_latency_mem_alloc' at .1. has no IMPLICIT type; did you mean 'omp_low_lat_mem_alloc'\\\?" "" { target *-*-* } .-1 }
    ! { dg-error "Expected integer expression of the 'omp_allocator_handle_kind' kind at .1." "" { target *-*-* } .-2 }
    i = 4
  !$omp end parallel
end
