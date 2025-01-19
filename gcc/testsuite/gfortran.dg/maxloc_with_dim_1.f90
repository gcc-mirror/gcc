! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline maxloc implementation,
! when the dim argument is present.

program p
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
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
  call check_lower_bounds
  call check_dependencies
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
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 51
    if (any(r /= reshape((/ real:: data1 /), (/ 4, 5 /)))) error stop 52
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 53
    if (any(r /= reshape((/ real:: data2 /), (/ 3, 5 /)))) error stop 54
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 55
    if (any(r /= reshape((/ real:: data3 /), (/ 3, 4 /)))) error stop 56
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 61
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 62
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 63
    if (any(r /= 0)) error stop 64
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 65
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 71
    if (any(r /= reshape((/ real:: data1 /), shape=(/ 4, 5 /)))) error stop 72
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 73
    if (any(r /= reshape((/ real:: data2 /), shape=(/ 3, 5 /)))) error stop 74
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 75
    if (any(r /= reshape((/ real:: data3 /), shape=(/ 3, 4 /)))) error stop 76
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) error stop 81
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) error stop 82
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) error stop 83
    if (any(r /= 0)) error stop 84
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) error stop 85
  end subroutine
  subroutine check_lower_bounds()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3:5,-1:2,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 91
    if (any(lbound(r) /= 1)) error stop 92
    if (any(ubound(r) /= (/ 4, 5 /))) error stop 93
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 94
    if (any(lbound(r) /= 1)) error stop 95
    if (any(ubound(r) /= (/ 3, 5 /))) error stop 96
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 97
    if (any(lbound(r) /= 1)) error stop 98
    if (any(ubound(r) /= (/ 3, 4 /))) error stop 99
  end subroutine
  elemental subroutine set(o, i)
    integer, intent(out) :: o
    integer, intent(in)  :: i
    o = i
  end subroutine
  subroutine check_dependencies()
    integer, allocatable :: a(:,:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    a(1,:,:) = maxloc(a, dim=1)
    if (any(a(1,:,:) /= reshape(data1, (/ 4, 5 /)))) error stop 111
    a(:,:,:) = reshape(data60, shape(a))
    a(:,2,:) = maxloc(a, dim=2)
    if (any(a(:,2,:) /= reshape(data2, (/ 3, 5 /)))) error stop 112
    a(:,:,:) = reshape(data60, shape(a))
    a(:,:,5) = maxloc(a, dim=3)
    if (any(a(:,:,5) /= reshape(data3, (/ 3, 4 /)))) error stop 113
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(1,:,:), maxloc(a, dim=1))
    if (any(a(1,:,:) /= reshape(data1, (/ 4, 5 /)))) error stop 114
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,2,:), maxloc(a, dim=2))
    if (any(a(:,2,:) /= reshape(data2, (/ 3, 5 /)))) error stop 115
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,:,5), maxloc(a, dim=3))
    if (any(a(:,:,5) /= reshape(data3, (/ 3, 4 /)))) error stop 116
  end subroutine check_dependencies
end program p
