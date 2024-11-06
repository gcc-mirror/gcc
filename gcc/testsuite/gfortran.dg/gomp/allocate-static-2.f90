module omp_lib_kinds
  use iso_c_binding, only: c_int, c_intptr_t
  implicit none
  private :: c_int, c_intptr_t
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
end module

module m
use omp_lib_kinds, only: omp_default_mem_alloc
implicit none
integer a,b
common /foo/ a,b  ! { dg-error "Sorry, !.OMP allocate for COMMON block variable 'foo' at .1. not supported" }
!$omp allocate(/foo/) align(128) allocator(omp_default_mem_alloc)
end

subroutine sub
use omp_lib_kinds
implicit none
integer a,b
common /foo/ a,b  ! { dg-error "Sorry, !.OMP allocate for COMMON block variable 'foo' at .1. not supported" }
!$omp allocate(/foo/) align(128) allocator(omp_default_mem_alloc)
end

subroutine outer
contains
subroutine inner
use omp_lib_kinds
implicit none
integer a,b
common /foo/ a,b  ! { dg-error "Sorry, !.OMP allocate for COMMON block variable 'foo' at .1. not supported" }
!$omp allocate(/foo/) align(128) allocator(omp_default_mem_alloc)
end
end
