! { dg-do compile }
! { dg-additional-options "-O -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?minloc" "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that all MINLOC and MAXLOC calls are inlined with optimizations,
! when ARRAY is of integral type, DIM is a constant, and MASK is absent.

subroutine check_maxloc
  implicit none
  integer, parameter :: data60(*) = (/ 2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1,  &
                                       2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1  /)
  integer, parameter :: data1(*) = (/ 2, 3, 2, 3,  &
                                      1, 2, 3, 2,  &
                                      3, 1, 2, 3,  &
                                      2, 3, 1, 2,  &
                                      3, 2, 3, 1  /)
  integer, parameter :: data2(*) = (/ 2, 1, 2,  &
                                      3, 2, 3,  &
                                      4, 3, 4,  &
                                      2, 1, 2,  &
                                      1, 2, 1  /)
  integer, parameter :: data3(*) = (/ 5, 1, 5,  &
                                      1, 2, 1,  &
                                      2, 1, 2,  &
                                      3, 2, 3  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 11
    if (any(r /= reshape(data1, (/ 4, 5 /)))) error stop 12
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 13
    if (any(r /= reshape(data2, (/ 3, 5 /)))) error stop 14
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 15
    if (any(r /= reshape(data3, (/ 3, 4 /)))) error stop 16
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 21
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 22
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 23
    if (any(r /= 0)) error stop 24
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 25
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 31
    if (any(r /= reshape(data1, (/ 4, 5 /)))) error stop 32
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 33
    if (any(r /= reshape(data2, (/ 3, 5 /)))) error stop 34
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 35
    if (any(r /= reshape(data3, (/ 3, 4 /)))) error stop 36
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 41
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 42
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 43
    if (any(r /= 0)) error stop 44
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 45
  end subroutine
end subroutine

subroutine check_minloc
  implicit none
  integer, parameter :: data60(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8,  &
                                       7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8  /)
  integer, parameter :: data1(*) = (/ 2, 3, 2, 3,  &
                                      1, 2, 3, 2,  &
                                      3, 1, 2, 3,  &
                                      2, 3, 1, 2,  &
                                      3, 2, 3, 1  /)
  integer, parameter :: data2(*) = (/ 2, 1, 2,  &
                                      3, 2, 3,  &
                                      4, 3, 4,  &
                                      2, 1, 2,  &
                                      1, 2, 1  /)
  integer, parameter :: data3(*) = (/ 5, 1, 5,  &
                                      1, 2, 1,  &
                                      2, 1, 2,  &
                                      3, 2, 3  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 111
    if (any(r /= reshape(data1, (/ 4, 5 /)))) error stop 112
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 113
    if (any(r /= reshape(data2, (/ 3, 5 /)))) error stop 114
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 115
    if (any(r /= reshape(data3, (/ 3, 4 /)))) error stop 116
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 121
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 122
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 123
    if (any(r /= 0)) error stop 124
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 125
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 131
    if (any(r /= reshape(data1, (/ 4, 5 /)))) error stop 132
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 133
    if (any(r /= reshape(data2, (/ 3, 5 /)))) error stop 134
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 135
    if (any(r /= reshape(data3, (/ 3, 4 /)))) error stop 136
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 141
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 142
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 143
    if (any(r /= 0)) error stop 144
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 145
  end subroutine
end subroutine
