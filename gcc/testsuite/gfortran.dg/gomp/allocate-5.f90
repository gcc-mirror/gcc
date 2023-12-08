! { dg-additional-options "-fopenmp-allocators" }
module my_omp_lib
  use iso_c_binding, only: c_intptr_t
  !use omp_lib
  implicit none
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
  type t
    integer :: a
  end type t
end module my_omp_lib

subroutine zero()
  !$omp assumes absent (allocators)

  !$omp assume absent (allocators)
  !$omp end assume
end

subroutine two(c,x2,y2)
  use my_omp_lib
  implicit none
  integer, allocatable :: a, b(:), c(:,:)
  type(t), allocatable :: x1
  type(t), pointer :: x2(:)
  class(t), allocatable :: y1
  class(t), pointer :: y2(:)

  !$omp flush  ! some executable statement
  !$omp allocate(a)
  allocate(a)
  deallocate(a)

  !$omp allocate(x1,y1,x2,y2)
  allocate(x1,y1,x2(5),y2(5))
  deallocate(x1,y1,x2,y2)

  !$omp allocate(b,a) align ( 128 )
  !$omp allocate align ( 64 )
  allocate(a,b(4),c(3,4))
  deallocate(a,b,c)
end

subroutine three(c)
  use my_omp_lib
  implicit none
  integer :: q
  integer, allocatable :: a, b(:), c(:,:)

  call foo()  ! executable stmt
  !$omp allocate allocator( omp_large_cap_mem_alloc ) , align(64)
  !$omp allocate(b) allocator( omp_high_bw_mem_alloc )
  !$omp allocate(c) allocator( omp_high_bw_mem_alloc )
  allocate(a,b(4),c(3,4))
  deallocate(a,b,c)

  block
    q = 5  ! executable stmt
    !$omp allocate(a) align(64)
    !$omp allocate(b) allocator( omp_high_bw_mem_alloc ), align(32)
    !$omp allocate(c) allocator( omp_thread_mem_alloc )
    allocate(a,b(4),c(3,4))
    deallocate(a,b,c)
  end block
  call inner
contains
  subroutine inner
    call foo()  ! executable stmt
    !$omp allocate(a) align(64)
    !$omp allocate(b) allocator( omp_high_bw_mem_alloc ), align(32)
    !$omp allocate(c) allocator( omp_thread_mem_alloc )
    allocate(a,b(4),c(3,4))
    deallocate(a,b,c)
  end subroutine inner
end
