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

subroutine bar (a, b, c)
  implicit none
  integer  :: a
  integer  :: b
  integer  :: c
  c = a + b
end

subroutine bar2 (a, b, c)
  implicit none
  integer  :: a
  integer  :: b(15)
  integer  :: c
  c = a + b(1)
end

subroutine foo(x, y)
  use omp_lib_kinds
  implicit none
  integer  :: x
  integer  :: z

  integer, dimension(15) :: y
  integer  :: r
  integer  :: i
  integer  c1, c2, c3, c4
  integer (kind=omp_allocator_handle_kind) :: h
  common /B1/ c1, c2
  common /B2/ c3, c4

  r = 0
  h = omp_default_mem_alloc;


  !$omp parallel private(/B1/, c3, c4) allocate(/B1/, /B2/)
  !$omp end parallel

  !$omp parallel private(/B1/, /B2/) allocate(h:/B1/, /B2/)
  !$omp end parallel
  
  !$omp parallel private(/B1/, /B2/) allocate(omp_large_cap_mem_alloc:/B1/, c3, c4)
  !$omp end parallel

  !$omp parallel allocate (x) allocate (h : y) &
  !$omp  allocate (omp_large_cap_mem_alloc:z) firstprivate (x, y, z)
  call bar2 (x, y, z);
  !$omp end parallel

  !$omp task private (x) firstprivate (z) allocate (omp_low_lat_mem_alloc:x,z)
  call bar (0, x, z);
  !$omp end task
  
  !$omp target teams distribute parallel do private (x) firstprivate (y) &
  !$omp allocate ((omp_default_mem_alloc + 0):z) allocate &
  !$omp (omp_default_mem_alloc: x, y) allocate (h: r) lastprivate (z) reduction(+:r)
  do i = 1, 10
    call bar (0, x, z);
    call bar2 (1, y, r);
  end do
  !$omp end target teams distribute parallel do

  !$omp single private (x) allocate (omp_low_lat_mem_alloc:x)
  x=1
  !$omp end single

  !$omp single allocate (omp_low_lat_mem_alloc:x) private (x)
  !$omp end single

  !$omp parallel
  !$omp do allocate (x) private (x)
  do i = 1, 64
    x = 1;
  end do
  !$omp end parallel

  !$omp sections private (x) allocate (omp_low_lat_mem_alloc: x)
    x = 1;
    !$omp section
    x = 2;
    !$omp section
    x = 3;
  !$omp end sections

  !$omp taskgroup task_reduction(+:r) allocate (omp_default_mem_alloc : r)
  call bar (r, r, r);
  !$omp end taskgroup

  !$omp teams private (x) firstprivate (y) allocate (h : x, y)
  call bar2 (x, y, r);
  !$omp end teams

  !$omp taskloop lastprivate (x) reduction (+:r) allocate (h : x, r)
  do i = 1, 16
    call bar (0, r, r);
    x = i;
  end do
  !$omp end taskloop

  !$omp taskgroup task_reduction(+:r) allocate (omp_default_mem_alloc : r)
  !$omp taskloop firstprivate (x) in_reduction (+:r) &
  !$omp allocate (omp_default_mem_alloc : x, r)
  do i = 1, 16
    call bar (x, r, r);
  end do
  !$omp end taskloop
  !$omp end taskgroup
  !$omp taskwait
end subroutine

