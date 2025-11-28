! { dg-do compile }
!
! PR fortran/122892
!
! OpenMP 6.0 clarified that the omp_{cgroup,pteam,thread}_mem_alloc
! (i.e. those with access trait != device) may only be used for
! static local variables.
! Check for this!

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

block data
  use omp_lib_kinds
  implicit none
  integer :: i1,i2,i3,i4,i5,i6,i7,i8
  common /b_i1/ i1
  common /b_i2/ i2
  common /b_i3/ i3
  common /b_i4/ i4
  common /b_i5/ i5
  common /b_i6/ i6
  common /b_i7/ i7
  common /b_i8/ i8

  data i1 / 1 /
  data i2 / 2 /
  data i3 / 3 /
  data i4 / 4 /
  data i5 / 5 /
  data i6 / 6 /
  data i7 / 7 /
  data i8 / 8 /

  !$omp allocate(/b_i1/) allocator(omp_default_mem_alloc)
  !$omp allocate(/b_i2/) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(/b_i3/) allocator(omp_const_mem_alloc)
  !$omp allocate(/b_i4/) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(/b_i5/) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(/b_i6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_i6/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_i7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_i7/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_i8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_i8/' at .2., may only be used for local static variables" }
end block data

block data my_block_data
  use omp_lib_kinds
  implicit none
  integer :: j1,j2,j3,j4,j5,j6,j7,j8
  common /b_j1/ j1
  common /b_j2/ j2
  common /b_j3/ j3
  common /b_j4/ j4
  common /b_j5/ j5
  common /b_j6/ j6
  common /b_j7/ j7
  common /b_j8/ j8

  data j1 / 1 /
  data j2 / 2 /
  data j3 / 3 /
  data j4 / 4 /
  data j5 / 5 /
  data j6 / 6 /
  data j7 / 7 /
  data j8 / 8 /

  !$omp allocate(/b_j1/) allocator(omp_default_mem_alloc)
  !$omp allocate(/b_j2/) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(/b_j3/) allocator(omp_const_mem_alloc)
  !$omp allocate(/b_j4/) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(/b_j5/) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(/b_j6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_j6/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_j7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_j7/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_j8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_j8/' at .2., may only be used for local static variables" }
end block data my_block_data

module m
  use omp_lib_kinds
  implicit none

  integer :: a1,a2,a3,a4,a5,a6,a7,a8
  integer :: b1,b2,b3,b4,b5,b6,b7,b8
  common /b_b1/ b1
  common /b_b2/ b2
  common /b_b3/ b3
  common /b_b4/ b4
  common /b_b5/ b5
  common /b_b6/ b6
  common /b_b7/ b7
  common /b_b8/ b8

  !$omp allocate(a1) allocator(omp_default_mem_alloc)
  !$omp allocate(a2) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(a3) allocator(omp_const_mem_alloc)
  !$omp allocate(a4) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(a5) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(a6) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item 'a6' at .2., may only be used for local static variables" }
  !$omp allocate(a7) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item 'a7' at .2., may only be used for local static variables" }
  !$omp allocate(a8) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item 'a8' at .2., may only be used for local static variables" }

  !$omp allocate(/b_b1/) allocator(omp_default_mem_alloc)
  !$omp allocate(/b_b2/) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(/b_b3/) allocator(omp_const_mem_alloc)
  !$omp allocate(/b_b4/) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(/b_b5/) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(/b_b6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_b6/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_b7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_b7/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_b8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_b8/' at .2., may only be used for local static variables" }
end

program main
  use omp_lib_kinds
  implicit none

  integer m1,m2,m3,m4,m5,m6,m7,m8
  integer n1,n2,n3,n4,n5,n6,n7,n8
  common /b_n1/ n1
  common /b_n2/ n2
  common /b_n3/ n3
  common /b_n4/ n4
  common /b_n5/ n5
  common /b_n6/ n6
  common /b_n7/ n7
  common /b_n8/ n8

  !$omp allocate(m1) allocator(omp_default_mem_alloc)
  !$omp allocate(m2) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(m3) allocator(omp_const_mem_alloc)
  !$omp allocate(m4) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(m5) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(m6) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item 'm6' at .2., may only be used for local static variables" }
  !$omp allocate(m7) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item 'm7' at .2., may only be used for local static variables" }
  !$omp allocate(m8) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item 'm8' at .2., may only be used for local static variables" }

  !$omp allocate(/b_n1/) allocator(omp_default_mem_alloc)
  !$omp allocate(/b_n2/) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(/b_n3/) allocator(omp_const_mem_alloc)
  !$omp allocate(/b_n4/) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(/b_n5/) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(/b_n6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_n6/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_n7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_n7/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_n8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_n8/' at .2., may only be used for local static variables" }

  block
    integer, save :: o1,o2,o3,o4,o5,o6,o7,o8
    ! NOTE: COMMON statement is not allowed inside of BLOCK

    !$omp allocate(o1) allocator(omp_default_mem_alloc)
    !$omp allocate(o2) allocator(omp_large_cap_mem_alloc)
    !$omp allocate(o3) allocator(omp_const_mem_alloc)
    !$omp allocate(o4) allocator(omp_high_bw_mem_alloc)
    !$omp allocate(o5) allocator(omp_low_lat_mem_alloc)
    !$omp allocate(o6) allocator(omp_cgroup_mem_alloc)
    !$omp allocate(o7) allocator(omp_pteam_mem_alloc)
    !$omp allocate(o8) allocator(omp_thread_mem_alloc)
  end block
end

subroutine sub
  use omp_lib_kinds
  implicit none

  integer, save :: s1,s2,s3,s4,s5,s6,s7,s8
  integer t1,t2,t3,t4,t5,t6,t7,t8
  common /b_t1/ t1
  common /b_t2/ t2
  common /b_t3/ t3
  common /b_t4/ t4
  common /b_t5/ t5
  common /b_t6/ t6
  common /b_t7/ t7
  common /b_t8/ t8

  !$omp allocate(s1) allocator(omp_default_mem_alloc)
  !$omp allocate(s2) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(s3) allocator(omp_const_mem_alloc)
  !$omp allocate(s4) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(s5) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(s6) allocator(omp_cgroup_mem_alloc)
  !$omp allocate(s7) allocator(omp_pteam_mem_alloc)
  !$omp allocate(s8) allocator(omp_thread_mem_alloc)

  !$omp allocate(/b_t1/) allocator(omp_default_mem_alloc)
  !$omp allocate(/b_t2/) allocator(omp_large_cap_mem_alloc)
  !$omp allocate(/b_t3/) allocator(omp_const_mem_alloc)
  !$omp allocate(/b_t4/) allocator(omp_high_bw_mem_alloc)
  !$omp allocate(/b_t5/) allocator(omp_low_lat_mem_alloc)
  !$omp allocate(/b_t6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_t6/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_t7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_t7/' at .2., may only be used for local static variables" }
  !$omp allocate(/b_t8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_t8/' at .2., may only be used for local static variables" }
contains
  integer function func()
    integer, save :: q1,q2,q3,q4,q5,q6,q7,q8
    integer r1,r2,r3,r4,r5,r6,r7,r8
    common /b_r1/ r1
    common /b_r2/ r2
    common /b_r3/ r3
    common /b_r4/ r4
    common /b_r5/ r5
    common /b_r6/ r6
    common /b_r7/ r7
    common /b_r8/ r8

    !$omp allocate(q1) allocator(omp_default_mem_alloc)
    !$omp allocate(q2) allocator(omp_large_cap_mem_alloc)
    !$omp allocate(q3) allocator(omp_const_mem_alloc)
    !$omp allocate(q4) allocator(omp_high_bw_mem_alloc)
    !$omp allocate(q5) allocator(omp_low_lat_mem_alloc)
    !$omp allocate(q6) allocator(omp_cgroup_mem_alloc)
    !$omp allocate(q7) allocator(omp_pteam_mem_alloc)
    !$omp allocate(q8) allocator(omp_thread_mem_alloc)

    !$omp allocate(/b_r1/) allocator(omp_default_mem_alloc)
    !$omp allocate(/b_r2/) allocator(omp_large_cap_mem_alloc)
    !$omp allocate(/b_r3/) allocator(omp_const_mem_alloc)
    !$omp allocate(/b_r4/) allocator(omp_high_bw_mem_alloc)
    !$omp allocate(/b_r5/) allocator(omp_low_lat_mem_alloc)
    !$omp allocate(/b_r6/) allocator(omp_cgroup_mem_alloc)  ! { dg-error "Predefined allocator 'omp_cgroup_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_r6/' at .2., may only be used for local static variables" }
    !$omp allocate(/b_r7/) allocator(omp_pteam_mem_alloc)   ! { dg-error "Predefined allocator 'omp_pteam_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_r7/' at .2., may only be used for local static variables" }
    !$omp allocate(/b_r8/) allocator(omp_thread_mem_alloc)  ! { dg-error "Predefined allocator 'omp_thread_mem_alloc' in ALLOCATOR clause at .1., used for list item '/b_r8/' at .2., may only be used for local static variables" }
  end function
end subroutine
