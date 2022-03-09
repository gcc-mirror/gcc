! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

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


subroutine foo(x, y, al)
  use omp_lib_kinds
  implicit none
  
type :: my_type
  integer :: i
  integer :: j
  real :: x
end type

  integer  :: x
  integer  :: y
  integer (kind=omp_allocator_handle_kind) :: al

  integer, allocatable :: var1
  integer, allocatable :: var2
  real, allocatable :: var3(:,:)
  type (my_type), allocatable :: var4
  integer, pointer :: pii, parr(:)

  character, allocatable :: str1a, str1aarr(:) 
  character(len=5), allocatable :: str5a, str5aarr(:)
  
  !$omp allocate
  allocate(str1a, str1aarr(10), str5a, str5aarr(10))

  !$omp allocate (var1) allocator(omp_default_mem_alloc)
  !$omp allocate (var2) allocator(omp_large_cap_mem_alloc)
  allocate (var1, var2)

  !$omp allocate (var4)  allocator(omp_low_lat_mem_alloc)
  allocate (var4)
  var4%i = 5

  !$omp allocate (var3)  allocator(omp_low_lat_mem_alloc)
  allocate (var3(x,y))

  !$omp allocate
  allocate(pii, parr(5))
end subroutine

! { dg-final { scan-tree-dump-times "#pragma omp allocate" 6 "original" } }
