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
end module my_omp_lib

subroutine one(n, my_alloc)
  use my_omp_lib
  implicit none
integer :: n
integer(kind=omp_allocator_handle_kind), intent(in) :: my_alloc

!stack variables:
integer :: a,b,c(n),d(5),e(2)
!$omp allocate(a)
!$omp allocate ( b , c ) align ( 32) allocator (my_alloc)
!$omp allocate (d) align( 128 )
!$omp allocate(   e ) allocator( omp_high_bw_mem_alloc )

!saved vars
integer, save :: k,l,m(5),r(2)  ! { dg-error "Sorry, !.OMP allocate for variable 'k' at .1. with SAVE attribute not yet implemented" }
!$omp allocate(k)  align(16) , allocator (omp_large_cap_mem_alloc)
!$omp allocate ( l ) allocator (omp_large_cap_mem_alloc) , align ( 32)
!$omp allocate (m) align( 128 ),allocator( omp_high_bw_mem_alloc )
!$omp allocate(   r ) allocator( omp_high_bw_mem_alloc )

!common /block/
integer :: q,x,y(2),z(5)
common /com1/ q,x
common /com2/ y,z
!$omp allocate ( / com1/) align( 128 ) allocator( omp_high_bw_mem_alloc )
!$omp allocate(/com2 / ) allocator( omp_high_bw_mem_alloc )
end
