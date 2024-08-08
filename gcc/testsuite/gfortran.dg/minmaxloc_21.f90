! { dg-do compile }
! { dg-additional-options "-O -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?minloc" "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that all MINLOC and MAXLOC calls are inlined with optimizations,
! when DIM is a constant, and either ARRAY has REAL type or MASK is non-scalar.

subroutine check_real_maxloc
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
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 1
    if (any(r /= reshape((/ real:: data1 /), (/ 4, 5 /)))) error stop 2
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 3
    if (any(r /= reshape((/ real:: data2 /), (/ 3, 5 /)))) error stop 4
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 5
    if (any(r /= reshape((/ real:: data3 /), (/ 3, 4 /)))) error stop 6
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 11
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 12
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 13
    if (any(r /= 0)) error stop 14
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 15
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 21
    if (any(r /= reshape((/ real:: data1 /), shape=(/ 4, 5 /)))) error stop 22
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 23
    if (any(r /= reshape((/ real:: data2 /), shape=(/ 3, 5 /)))) error stop 24
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 25
    if (any(r /= reshape((/ real:: data3 /), shape=(/ 3, 4 /)))) error stop 26
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 31
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 32
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 33
    if (any(r /= 0)) error stop 34
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 35
  end subroutine
end subroutine

subroutine check_maxloc_with_mask
  implicit none
  integer, parameter :: data60(*) = (/ 2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1,  &
                                       2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1  /)
  logical, parameter :: mask60(*) = (/ .true. , .false., .false., .false., &
                                       .true. , .false., .true. , .false., &
                                       .false., .true. , .true. , .false., &
                                       .true. , .true. , .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .true. , &
                                       .true. , .false., .false., .true. , &
                                       .true. , .true. , .true. , .false., &
                                       .false., .false., .true. , .false., &
                                       .true. , .false., .true. , .true. , &
                                       .true. , .false., .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .false., &
                                       .false., .true. , .true. , .true. , &
                                       .false., .true. , .false., .true.  /)
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
  integer, parameter :: data1m(*) = (/ 1, 2, 1, 1,  &
                                       1, 3, 2, 3,  &
                                       1, 1, 1, 2,  &
                                       3, 1, 1, 3,  &
                                       2, 3, 1, 1  /)
  integer, parameter :: data2m(*) = (/ 4, 4, 0,  &
                                       1, 1, 2,  &
                                       1, 2, 2,  &
                                       2, 3, 1,  &
                                       3, 3, 2  /)
  integer, parameter :: data3m(*) = (/ 3, 2, 4,  &
                                       4, 3, 2,  &
                                       5, 4, 0,  &
                                       1, 1, 2  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 41
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 42
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 43
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 44
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 45
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 46
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 51
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 52
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 53
    if (any(r /= 0)) error stop 54
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 55
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 61
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 62
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 63
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 64
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 65
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 66
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 71
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 72
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 73
    if (any(r /= 0)) error stop 74
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 75
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 81
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 82
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 83
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 84
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 85
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 86
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 91
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 92
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 93
    if (any(r /= 0)) error stop 94
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 95
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 101
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 102
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 103
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 104
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 105
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 106
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 111
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 112
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 113
    if (any(r /= 0)) error stop 114
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 115
  end subroutine
end subroutine

subroutine check_real_minloc
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
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 141
    if (any(r /= reshape((/ real:: data1 /), (/ 4, 5 /)))) error stop 142
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 143
    if (any(r /= reshape((/ real:: data2 /), (/ 3, 5 /)))) error stop 144
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 145
    if (any(r /= reshape((/ real:: data3 /), (/ 3, 4 /)))) error stop 146
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 151
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 152
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 153
    if (any(r /= 0)) error stop 154
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 155
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 161
    if (any(r /= reshape((/ real:: data1 /), shape=(/ 4, 5 /)))) error stop 162
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 163
    if (any(r /= reshape((/ real:: data2 /), shape=(/ 3, 5 /)))) error stop 164
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 165
    if (any(r /= reshape((/ real:: data3 /), shape=(/ 3, 4 /)))) error stop 166
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 171
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 172
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 173
    if (any(r /= 0)) error stop 174
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 175
  end subroutine
end subroutine

subroutine check_minloc_with_mask
  implicit none
  integer, parameter :: data60(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8,  &
                                       7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8  /)
  logical, parameter :: mask60(*) = (/ .true. , .false., .false., .false., &
                                       .true. , .false., .true. , .false., &
                                       .false., .true. , .true. , .false., &
                                       .true. , .true. , .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .true. , &
                                       .true. , .false., .false., .true. , &
                                       .true. , .true. , .true. , .false., &
                                       .false., .false., .true. , .false., &
                                       .true. , .false., .true. , .true. , &
                                       .true. , .false., .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .false., &
                                       .false., .true. , .true. , .true. , &
                                       .false., .true. , .false., .true.  /)
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
  integer, parameter :: data1m(*) = (/ 1, 2, 1, 1,  &
                                       1, 3, 2, 3,  &
                                       1, 1, 1, 2,  &
                                       3, 1, 1, 3,  &
                                       2, 3, 1, 1  /)
  integer, parameter :: data2m(*) = (/ 4, 4, 0,  &
                                       1, 1, 2,  &
                                       1, 2, 2,  &
                                       2, 3, 1,  &
                                       3, 3, 2  /)
  integer, parameter :: data3m(*) = (/ 3, 2, 4,  &
                                       4, 3, 2,  &
                                       5, 4, 0,  &
                                       1, 1, 2  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
  call check_lower_bounds
  call check_dependencies
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 181
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 182
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 183
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 184
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 185
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 186
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 191
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 192
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 193
    if (any(r /= 0)) error stop 194
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 195
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 201
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 202
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 203
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 204
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 205
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 206
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 211
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 212
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 213
    if (any(r /= 0)) error stop 214
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 215
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 221
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 222
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 223
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 224
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 225
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 226
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 231
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 232
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 233
    if (any(r /= 0)) error stop 234
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 235
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 241
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) error stop 242
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 243
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) error stop 244
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 245
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) error stop 246
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 251
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 252
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 253
    if (any(r /= 0)) error stop 254
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 255
  end subroutine
end subroutine
