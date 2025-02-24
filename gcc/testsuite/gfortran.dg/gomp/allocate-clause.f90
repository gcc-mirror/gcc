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

subroutine test1 ()
  use omp_lib_kinds
  implicit none
  integer :: x(5), i

  !$omp parallel allocate(omp_thread_mem_alloc: x) firstprivate(x)
   x(1) = 1
  !$omp end parallel

  !$omp target allocate(omp_thread_mem_alloc: x) firstprivate(x) ! uses_allocators(omp_thread_mem_alloc)
    ! { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'target' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
    x(1) = 1
  !$omp end target

  !$omp taskloop allocate(omp_thread_mem_alloc: x) firstprivate(x)
   ! { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
   do i = 1, 5
     x(i) = i
   end do

  !$omp parallel master taskloop simd allocate(omp_thread_mem_alloc: x) firstprivate(x)
   ! { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'taskloop' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
   do i = 1, 5
     x(i) = i
   end do

  !$omp parallel
  !$omp masked
    !$omp task allocate(omp_thread_mem_alloc: x) firstprivate(x)
      ! { dg-warning "allocator with access trait set to 'thread' results in undefined behavior for 'task' directive \\\[-Wopenmp\\\]" "" { target *-*-* } .-1 }
      x(1) = 1
    !$omp end task
  !$omp end masked
  !$omp end parallel
end
