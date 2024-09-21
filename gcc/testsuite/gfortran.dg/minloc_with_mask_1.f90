! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline MINLOC implementation,
! when there is a mask argument.

program p
  implicit none
  integer, parameter :: data10(*) = (/ 2, 5, 2, 3, 3, 5, 3, 6, 0, 1 /)
  logical, parameter :: mask10(*) = (/ .false., .true., .false., &
                                       .false., .true., .true.,  &
                                       .true. , .true., .false., &
                                       .false. /)
  integer, parameter :: data64(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4,  &
                                       5, 5, 8, 2, 6, 7, 8, 7,  &
                                       4, 5, 3, 9, 0, 6, 4, 5,  &
                                       5, 8, 2, 6, 7, 8, 7, 4,  &
                                       5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5,  &
                                       3, 9, 0, 6, 4, 5, 5, 8,  &
                                       2, 6, 7, 8, 7, 4, 5, 3  /)
  logical, parameter :: mask64(*) = (/ .true. , .false., .false., .false., &
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
                                       .false., .true. , .false., .true. , &
                                       .true. , .false., .false., .false. /)
  call check_int_const_shape_rank_1
  call check_int_const_shape_rank_3
  call check_int_const_shape_rank_3_true_mask
  call check_int_const_shape_rank_3_false_mask
  call check_int_const_shape_rank_3_optional_mask_present
  call check_int_const_shape_rank_3_optional_mask_absent
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_1
  call check_int_alloc_rank_3
  call check_int_alloc_rank_3_true_mask
  call check_int_alloc_rank_3_false_mask
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_1
  call check_real_const_shape_rank_3
  call check_real_const_shape_rank_3_true_mask
  call check_real_const_shape_rank_3_false_mask
  call check_real_const_shape_rank_3_optional_mask_present
  call check_real_const_shape_rank_3_optional_mask_absent
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_1
  call check_real_alloc_rank_3
  call check_real_alloc_rank_3_true_mask
  call check_real_alloc_rank_3_false_mask
  call check_real_alloc_empty_4
  call check_lower_bounds
  call check_dependencies
contains
  subroutine check_int_const_shape_rank_1()
    integer :: a(10)
    logical :: m(10)
    integer, allocatable :: r(:)
    a = data10
    m = mask10
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 1) stop 11
    if (any(r /= (/ 5 /))) stop 12
  end subroutine
  subroutine check_int_const_shape_rank_3()
    integer :: a(4,4,4)
    logical :: m(4,4,4)
    integer, allocatable :: r(:)
    a = reshape(data64, shape(a))
    m = reshape(mask64, shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 21
    if (any(r /= (/ 2, 3, 1 /))) stop 22
  end subroutine
  subroutine check_int_const_shape_rank_3_true_mask()
    integer :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape(data64, shape(a))
    r = minloc(a, mask = .true.)
    if (size(r, dim = 1) /= 3) stop 31
    if (any(r /= (/ 2, 2, 1 /))) stop 32
  end subroutine
  subroutine check_int_const_shape_rank_3_false_mask()
    integer :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape(data64, shape(a))
    r = minloc(a, mask = .false.)
    if (size(r, dim = 1) /= 3) stop 41
    if (any(r /= (/ 0, 0, 0 /))) stop 42
  end subroutine
  subroutine call_minloc_int(r, a, m)
    integer :: a(:,:,:)
    logical, optional :: m(:,:,:)
    integer, allocatable :: r(:)
    r = minloc(a, mask = m)
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_present()
    integer :: a(4,4,4)
    logical :: m(4,4,4)
    integer, allocatable :: r(:)
    a = reshape(data64, shape(a))
    m = reshape(mask64, shape(m))
    call call_minloc_int(r, a, m)
    if (size(r, dim = 1) /= 3) stop 51
    if (any(r /= (/ 2, 3, 1 /))) stop 52
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_absent()
    integer :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape(data64, shape(a))
    call call_minloc_int(r, a)
    if (size(r, dim = 1) /= 3) stop 61
    if (any(r /= (/ 2, 2, 1 /))) stop 62
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 4) stop 71
    if (any(r /= (/ 0, 0, 0, 0 /))) stop 72
  end subroutine
  subroutine check_int_alloc_rank_1()
    integer, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer, allocatable :: r(:)
    allocate(a(10), m(10))
    a(:) = data10
    m(:) = mask10
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 1) stop 81
    if (any(r /= (/ 5 /))) stop 82
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4), m(4,4,4))
    a(:,:,:) = reshape(data64, shape(a))
    m(:,:,:) = reshape(mask64, shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 91
    if (any(r /= (/ 2, 3, 1 /))) stop 92
  end subroutine
  subroutine check_int_alloc_rank_3_true_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape(data64, shape(a))
    r = minloc(a, mask = .true.)
    if (size(r, dim = 1) /= 3) stop 101
    if (any(r /= (/ 2, 2, 1 /))) stop 102
  end subroutine
  subroutine check_int_alloc_rank_3_false_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape(data64, shape(a))
    r = minloc(a, mask = .false.)
    if (size(r, dim = 1) /= 3) stop 111
    if (any(r /= (/ 0, 0, 0 /))) stop 112
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 4) stop 121
    if (any(r /= (/ 0, 0, 0, 0 /))) stop 122
  end subroutine
  subroutine check_real_const_shape_rank_1()
    real :: a(10)
    logical :: m(10)
    integer, allocatable :: r(:)
    a = (/ real:: data10 /)
    m = mask10
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 1) stop 131
    if (any(r /= (/ 5 /))) stop 132
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(4,4,4)
    logical :: m(4,4,4)
    integer, allocatable :: r(:)
    a = reshape((/ real:: data64 /), shape(a))
    m = reshape(mask64, shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 141
    if (any(r /= (/ 2, 3, 1 /))) stop 142
  end subroutine
  subroutine check_real_const_shape_rank_3_true_mask()
    real :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape((/ real:: data64 /), shape(a))
    r = minloc(a, mask = .true.)
    if (size(r, dim = 1) /= 3) stop 151
    if (any(r /= (/ 2, 2, 1 /))) stop 152
  end subroutine
  subroutine check_real_const_shape_rank_3_false_mask()
    real :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape((/ real:: data64 /), shape(a))
    r = minloc(a, mask = .false.)
    if (size(r, dim = 1) /= 3) stop 161
    if (any(r /= (/ 0, 0, 0 /))) stop 162
  end subroutine
  subroutine call_minloc_real(r, a, m)
    real :: a(:,:,:)
    logical, optional  :: m(:,:,:)
    integer, allocatable :: r(:)
    r = minloc(a, mask = m)
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_present()
    real :: a(4,4,4)
    logical :: m(4,4,4)
    integer, allocatable :: r(:)
    a = reshape((/ real:: data64 /), shape(a))
    m = reshape(mask64, shape(m))
    call call_minloc_real(r, a, m)
    if (size(r, dim = 1) /= 3) stop 171
    if (any(r /= (/ 2, 3, 1 /))) stop 172
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_absent()
    real :: a(4,4,4)
    integer, allocatable :: r(:)
    a = reshape((/ real:: data64 /), shape(a))
    call call_minloc_real(r, a)
    if (size(r, dim = 1) /= 3) stop 181
    if (any(r /= (/ 2, 2, 1 /))) stop 182
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 4) stop 191
    if (any(r /= (/ 0, 0, 0, 0 /))) stop 192
  end subroutine
  subroutine check_real_alloc_rank_1()
    real, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer, allocatable :: r(:)
    allocate(a(10), m(10))
    a(:) = (/ real:: data10 /)
    m(:) = mask10
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 1) stop 201
    if (any(r /= (/ 5 /))) stop 202
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4), m(4,4,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    m(:,:,:) = reshape(mask64, shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 211
    if (any(r /= (/ 2, 3, 1 /))) stop 212
  end subroutine
  subroutine check_real_alloc_rank_3_true_mask()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    r = minloc(a, mask = .true.)
    if (size(r, dim = 1) /= 3) stop 221
    if (any(r /= (/ 2, 2, 1 /))) stop 222
  end subroutine
  subroutine check_real_alloc_rank_3_false_mask()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(4,4,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    r = minloc(a, mask = .false.)
    if (size(r, dim = 1) /= 3) stop 231
    if (any(r /= (/ 0, 0, 0 /))) stop 232
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 4) stop 241
    if (any(r /= (/ 0, 0, 0, 0 /))) stop 242
  end subroutine
  subroutine check_lower_bounds()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:)
    allocate(a(3:6,-1:2,4), m(3:6,-1:2,4))
    a(:,:,:) = reshape((/ real:: data64 /), shape(a))
    m = reshape(mask64, shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 251
    if (any(r /= (/ 2, 3, 1 /))) stop 252
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
    a(1,1,:) = minloc(a, mask=a<8)
    if (any(a(1,1,:) /= (/ 3, 2, 1 /))) stop 171
    a(:,:,:) = reshape(data64(2:28), shape(a))
    a(3,3,:) = minloc(a, mask=a<8)
    if (any(a(3,3,:) /= (/ 2, 2, 1 /))) stop 172
    a(:,:,:) = reshape(data64(3:29), shape(a))
    a(1,:,1) = minloc(a, mask=a<8)
    if (any(a(1,:,1) /= (/ 1, 2, 1 /))) stop 173
    a(:,:,:) = reshape(data64(5:31), shape(a))
    a(2,:,2) = minloc(a, mask=a<8)
    if (any(a(2,:,2) /= (/ 2, 1, 1 /))) stop 174
    a(:,:,:) = reshape(data64(6:32), shape(a))
    a(3,:,3) = minloc(a, mask=a<8)
    if (any(a(3,:,3) /= (/ 1, 1, 1 /))) stop 175
    a(:,:,:) = reshape(data64(7:33), shape(a))
    a(:,1,1) = minloc(a, mask=a<8)
    if (any(a(:,1,1) /= (/ 3, 2, 2 /))) stop 176
    a(:,:,:) = reshape(data64(8:34), shape(a))
    a(:,3,3) = minloc(a, mask=a<8)
    if (any(a(:,3,3) /= (/ 2, 2, 2 /))) stop 177
    ! Subroutine assignment
    a(:,:,:) = reshape(data64(9:35), shape(a))
    call set(a(1,1,:), minloc(a, mask=a<8))
    if (any(a(1,1,:) /= (/ 1, 2, 2 /))) stop 181
    a(:,:,:) = reshape(data64(10:36), shape(a))
    call set(a(3,3,:), minloc(a, mask=a<8))
    if (any(a(3,3,:) /= (/ 3, 1, 2 /))) stop 182
    a(:,:,:) = reshape(data64(11:37), shape(a))
    call set(a(1,:,1), minloc(a, mask=a<8))
    if (any(a(1,:,1) /= (/ 2, 1, 2 /))) stop 183
    a(:,:,:) = reshape(data64(12:38), shape(a))
    call set(a(2,:,2), minloc(a, mask=a<8))
    if (any(a(2,:,2) /= (/ 1, 1, 2 /))) stop 184
    a(:,:,:) = reshape(data64(13:39), shape(a))
    call set(a(3,:,3), minloc(a, mask=a<8))
    if (any(a(3,:,3) /= (/ 3, 3, 1 /))) stop 185
    a(:,:,:) = reshape(data64(14:40), shape(a))
    call set(a(:,1,1), minloc(a, mask=a<8))
    if (any(a(:,1,1) /= (/ 2, 3, 1 /))) stop 186
    a(:,:,:) = reshape(data64(15:41), shape(a))
    call set(a(:,3,3), minloc(a, mask=a<8))
    if (any(a(:,3,3) /= (/ 1, 3, 1 /))) stop 187
    call set(a(1,:,:), minloc(a, dim=1))
  end subroutine check_dependencies
end program p
