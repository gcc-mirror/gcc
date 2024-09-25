! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline MINLOC implementation,
! when there is no optional argument.

program p
  implicit none
  integer, parameter :: data5(*) = (/ 8, 2, 7, 2, 9 /)
  integer, parameter :: data64(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4,  &
                                       5, 5, 8, 2, 6, 7, 8, 7,  &
                                       4, 5, 3, 9, 0, 6, 4, 5,  &
                                       5, 8, 2, 6, 7, 8, 7, 4,  &
                                       5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5,  &
                                       3, 9, 0, 6, 4, 5, 5, 8,  &
                                       2, 6, 7, 8, 7, 4, 5, 3  /)
  call check_int_const_shape_rank_1
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_1
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_1
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_1
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
  call check_int_lower_bounds
  call check_real_lower_bounds
  call check_dependencies
contains
  subroutine check_int_const_shape_rank_1()
    integer :: a(5)
    integer, allocatable :: m(:)
    a = data5
    m = minloc(a)
    if (size(m, dim=1) /= 1) stop 11
    if (any(m /= (/ 2 /))) stop 12
  end subroutine
  subroutine check_int_const_shape_rank_3()
    integer :: a(4,4,4)
    integer, allocatable :: m(:)
    a = reshape(data64, shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 21
    if (any(m /= (/ 2, 2, 1 /))) stop 22
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    integer, allocatable :: m(:)
    a = reshape((/ integer:: /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 4) stop 31
    if (any(m /= (/ 0, 0, 0, 0 /))) stop 32
  end subroutine
  subroutine check_int_alloc_rank_1()
    integer, allocatable :: a(:)
    integer, allocatable :: m(:)
    allocate(a(5))
    a(:) = data5
    m = minloc(a)
    if (size(m, dim=1) /= 1) stop 41
    if (any(m /= (/ 2 /))) stop 42
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: m(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape(data64, shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 51
    if (any(m /= (/ 2, 2, 1 /))) stop 52
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    integer, allocatable :: m(:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 4) stop 61
    if (any(m /= (/ 0, 0, 0, 0 /))) stop 62
  end subroutine
  subroutine check_real_const_shape_rank_1()
    real :: a(5)
    integer, allocatable :: m(:)
    a = (/ real:: data5 /)
    m = minloc(a)
    if (size(m, dim=1) /= 1) stop 71
    if (any(m /= (/ 2 /))) stop 72
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(4,4,4)
    integer, allocatable :: m(:)
    a = reshape((/ real:: data64 /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 81
    if (any(m /= (/ 2, 2, 1 /))) stop 82
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: m(:)
    a = reshape((/ real:: /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 4) stop 91
    if (any(m /= (/ 0, 0, 0, 0 /))) stop 92
  end subroutine
  subroutine check_real_alloc_rank_1()
    real, allocatable :: a(:)
    integer, allocatable :: m(:)
    allocate(a(5))
    a(:) = (/ real:: data5 /)
    m = minloc(a)
    if (size(m, dim=1) /= 1) stop 111
    if (any(m /= (/ 2 /))) stop 112
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: m(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 121
    if (any(m /= (/ 2, 2, 1 /))) stop 122
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: m(:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 4) stop 131
    if (any(m /= (/ 0, 0, 0, 0 /))) stop 132
  end subroutine
  subroutine check_int_lower_bounds()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: m(:)
    allocate(a(3:6,-1:2,4))
    a(:,:,:) = reshape(data64, shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 141
    if (any(m /= (/ 2, 2, 1 /))) stop 142
  end subroutine
  subroutine check_real_lower_bounds()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: m(:)
    allocate(a(3:6,-1:2,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 151
    if (any(m /= (/ 2, 2, 1 /))) stop 152
  end subroutine
  elemental subroutine set(o, i)
    integer, intent(out) :: o
    integer, intent(in)  :: i
    o = i
  end subroutine
  subroutine check_dependencies()
    integer, allocatable :: a(:,:,:)
    allocate(a(3,3,3))
    ! Direct assignment
    a(:,:,:) = reshape(data64(1:27), shape(a))
    a(1,1,:) = minloc(a)
    if (any(a(1,1,:) /= (/ 3, 2, 1 /))) stop 171
    a(:,:,:) = reshape(data64(2:28), shape(a))
    a(3,3,:) = minloc(a)
    if (any(a(3,3,:) /= (/ 2, 2, 1 /))) stop 172
    a(:,:,:) = reshape(data64(3:29), shape(a))
    a(1,:,1) = minloc(a)
    if (any(a(1,:,1) /= (/ 1, 2, 1 /))) stop 173
    a(:,:,:) = reshape(data64(5:31), shape(a))
    a(2,:,2) = minloc(a)
    if (any(a(2,:,2) /= (/ 2, 1, 1 /))) stop 174
    a(:,:,:) = reshape(data64(6:32), shape(a))
    a(3,:,3) = minloc(a)
    if (any(a(3,:,3) /= (/ 1, 1, 1 /))) stop 175
    a(:,:,:) = reshape(data64(7:33), shape(a))
    a(:,1,1) = minloc(a)
    if (any(a(:,1,1) /= (/ 3, 2, 2 /))) stop 176
    a(:,:,:) = reshape(data64(8:34), shape(a))
    a(:,3,3) = minloc(a)
    if (any(a(:,3,3) /= (/ 2, 2, 2 /))) stop 177
    ! Subroutine assignment
    a(:,:,:) = reshape(data64(9:35), shape(a))
    call set(a(1,1,:), minloc(a))
    if (any(a(1,1,:) /= (/ 1, 2, 2 /))) stop 181
    a(:,:,:) = reshape(data64(10:36), shape(a))
    call set(a(3,3,:), minloc(a))
    if (any(a(3,3,:) /= (/ 3, 1, 2 /))) stop 182
    a(:,:,:) = reshape(data64(11:37), shape(a))
    call set(a(1,:,1), minloc(a))
    if (any(a(1,:,1) /= (/ 2, 1, 2 /))) stop 183
    a(:,:,:) = reshape(data64(12:38), shape(a))
    call set(a(2,:,2), minloc(a))
    if (any(a(2,:,2) /= (/ 1, 1, 2 /))) stop 184
    a(:,:,:) = reshape(data64(13:39), shape(a))
    call set(a(3,:,3), minloc(a))
    if (any(a(3,:,3) /= (/ 3, 3, 1 /))) stop 185
    a(:,:,:) = reshape(data64(14:40), shape(a))
    call set(a(:,1,1), minloc(a))
    if (any(a(:,1,1) /= (/ 2, 3, 1 /))) stop 186
    a(:,:,:) = reshape(data64(15:41), shape(a))
    call set(a(:,3,3), minloc(a))
    if (any(a(:,3,3) /= (/ 1, 3, 1 /))) stop 187
    call set(a(1,:,:), minloc(a, dim=1))
  end subroutine check_dependencies
end program p
