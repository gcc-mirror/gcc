! { dg-do compile }

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

subroutine foo(x, y)
  use omp_lib_kinds
  implicit none
  integer  :: x
  integer  :: y

  integer, allocatable :: var1(:)
  integer, allocatable :: var2(:)
  integer, allocatable :: var3(:)
  integer, allocatable :: var4(:,:)
  integer, allocatable :: var5(:)
  integer, allocatable :: var6(:)
  integer, allocatable :: var7(:)
  integer, allocatable :: var8(:)
  integer, allocatable :: var9(:)
  integer, allocatable :: var10(:)
  integer, allocatable :: var11(:)
  integer, allocatable :: var12(:)

  !$omp allocate (var1) allocator(omp_default_mem_alloc)
  allocate (var1(x))
  
  !$omp allocate (var2)
  allocate (var2(x))

  !$omp allocate (var3, var4) allocator(omp_large_cap_mem_alloc)
  allocate (var3(x),var4(x,y))

  !$omp allocate()
  allocate (var5(x))

  !$omp allocate
  allocate (var6(x))

  !$omp allocate () allocator(omp_default_mem_alloc)
  allocate (var7(x))

  !$omp allocate allocator(omp_default_mem_alloc)
  allocate (var8(x))

  !$omp allocate (var9) allocator(omp_default_mem_alloc)
  !$omp allocate (var10) allocator(omp_large_cap_mem_alloc)
  allocate (var9(x), var10(x))

end subroutine
