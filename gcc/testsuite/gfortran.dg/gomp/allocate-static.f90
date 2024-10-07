! { dg-do run }

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
  use iso_c_binding, only: c_intptr_t
  use omp_lib_kinds, only: omp_default_mem_alloc
  implicit none (type, external)

  integer(c_intptr_t) :: intptr

  integer :: A(4) = [1,2,3,4]
  !$omp allocate(A) align(128) allocator(omp_default_mem_alloc)
contains
  subroutine f()
    integer :: B(4) = [1,2,3,4]
    !$omp allocate(B) align(256) allocator(omp_default_mem_alloc)

    if (mod (transfer (loc (A), intptr), 128_c_intptr_t) /= 0) stop 1
    if (mod (transfer (loc (B), intptr), 256_c_intptr_t) /= 0) stop 2

    call inner()
  contains
    subroutine inner()
      integer :: C(4) = [1,2,3,4]
      !$omp allocate(C) align(1024) allocator(omp_default_mem_alloc)
      if (mod (transfer (loc (A), intptr), 128_c_intptr_t) /= 0) stop 3
      if (mod (transfer (loc (B), intptr), 256_c_intptr_t) /= 0) stop 4
      if (mod (transfer (loc (C), intptr), 1024_c_intptr_t) /= 0) stop 5
    end
  end
end

use m
implicit none (type, external)
if (mod (transfer (loc (A), intptr), 128_c_intptr_t) /= 0) stop 6
call f()
end
