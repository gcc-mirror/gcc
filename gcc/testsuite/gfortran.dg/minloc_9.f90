! { dg-do run }
!
! PR fortran/90608
! Check that the evaluation of MINLOC's BACK argument is made only once
! before the scalarization loops, when the DIM argument is present.

program p
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
  integer :: calls_count = 0
  call check_int_const_shape_rank_3
  call check_int_const_shape_rank_3_scalar_mask
  call check_int_const_shape_rank_3_optional_mask_present
  call check_int_const_shape_rank_3_optional_mask_absent
  call check_int_const_shape_rank_3_array_mask
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_rank_3_scalar_mask
  call check_int_alloc_rank_3_array_mask
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_rank_3_scalar_mask
  call check_real_const_shape_rank_3_optional_mask_present
  call check_real_const_shape_rank_3_optional_mask_absent
  call check_real_const_shape_rank_3_array_mask
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_rank_3_scalar_mask
  call check_real_alloc_rank_3_array_mask
  call check_real_alloc_empty_4
contains
  function get_scalar_false()
    logical :: get_scalar_false
    calls_count = calls_count + 1
    get_scalar_false = .false.
  end function
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    calls_count = 0
    r = minloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) error stop 12
    r = minloc(a, dim = 2, back = get_scalar_false())
    if (calls_count /= 2) error stop 15
    r = minloc(a, dim = 3, back = get_scalar_false())
    if (calls_count /= 3) error stop 18
  end subroutine
  subroutine check_int_const_shape_rank_3_scalar_mask()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by minloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = minloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) error stop 22
    r = minloc(a, dim = 2, mask = .true., back = get_scalar_false())
    if (calls_count /= 2) error stop 25
    r = minloc(a, dim = 3, mask = .true., back = get_scalar_false())
    if (calls_count /= 3) error stop 28
  end subroutine
  subroutine call_minloc_int(r, a, m, b)
    integer :: a(:,:,:)
    logical, optional :: m(:,:,:)
    logical, optional :: b
    integer, allocatable :: r(:,:)
    r = minloc(a, dim = 2, mask = m, back = b)
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_present()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    calls_count = 0
    call call_minloc_int(r, a, m, get_scalar_false())
    if (calls_count /= 1) error stop 45
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_absent()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    calls_count = 0
    call call_minloc_int(r, a, b = get_scalar_false())
    if (calls_count /= 1) error stop 55
  end subroutine
  subroutine check_int_const_shape_rank_3_array_mask()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 62
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 65
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 68
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 72
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 74
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 76
    r = minloc(a, dim = 4, mask = m, back = get_scalar_false())
    if (calls_count /= 4) error stop 78
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    calls_count = 0
    r = minloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) error stop 82
    r = minloc(a, dim = 2, back = get_scalar_false())
    if (calls_count /= 2) error stop 85
    r = minloc(a, dim = 3, back = get_scalar_false())
    if (calls_count /= 3) error stop 88
  end subroutine
  subroutine check_int_alloc_rank_3_scalar_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by minloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = minloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) error stop 92
    r = minloc(a, dim = 2, mask = .true., back = get_scalar_false())
    if (calls_count /= 2) error stop 95
    r = minloc(a, dim = 3, mask = .true., back = get_scalar_false())
    if (calls_count /= 3) error stop 98
  end subroutine
  subroutine check_int_alloc_rank_3_array_mask()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 102
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 105
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 108
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 112
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 114
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 116
    r = minloc(a, dim = 4, mask = m, back = get_scalar_false())
    if (calls_count /= 4) error stop 118
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) error stop 122
    r = minloc(a, dim = 2, back = get_scalar_false())
    if (calls_count /= 2) error stop 125
    r = minloc(a, dim = 3, back = get_scalar_false())
    if (calls_count /= 3) error stop 128
  end subroutine
  subroutine check_real_const_shape_rank_3_scalar_mask()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by minloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = minloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) error stop 132
    r = minloc(a, dim = 2, mask = .true., back = get_scalar_false())
    if (calls_count /= 2) error stop 135
    r = minloc(a, dim = 3, mask = .true., back = get_scalar_false())
    if (calls_count /= 3) error stop 138
  end subroutine
  subroutine check_real_const_shape_rank_3_array_mask()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 142
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 145
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 148
  end subroutine
  subroutine call_minloc_real(r, a, m, b)
    real :: a(:,:,:)
    logical, optional :: m(:,:,:)
    logical, optional :: b
    integer, allocatable :: r(:,:)
    r = minloc(a, dim = 2, mask = m, back = b)
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_present()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    calls_count = 0
    call call_minloc_real(r, a, m, get_scalar_false())
    if (calls_count /= 1) error stop 155
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_absent()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    calls_count = 0
    call call_minloc_real(r, a, b = get_scalar_false())
    if (calls_count /= 1) error stop 165
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 172
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 174
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 176
    r = minloc(a, dim = 4, mask = m, back = get_scalar_false())
    if (calls_count /= 4) error stop 178
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    calls_count = 0
    r = minloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) error stop 182
    r = minloc(a, dim = 2, back = get_scalar_false())
    if (calls_count /= 2) error stop 185
    r = minloc(a, dim = 3, back = get_scalar_false())
    if (calls_count /= 3) error stop 188
  end subroutine
  subroutine check_real_alloc_rank_3_scalar_mask()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by minloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = minloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) error stop 192
    r = minloc(a, dim = 2, mask = .true., back = get_scalar_false())
    if (calls_count /= 2) error stop 195
    r = minloc(a, dim = 3, mask = .true., back = get_scalar_false())
    if (calls_count /= 3) error stop 198
  end subroutine
  subroutine check_real_alloc_rank_3_array_mask()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 202
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 205
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 208
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    calls_count = 0
    r = minloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) error stop 212
    r = minloc(a, dim = 2, mask = m, back = get_scalar_false())
    if (calls_count /= 2) error stop 214
    r = minloc(a, dim = 3, mask = m, back = get_scalar_false())
    if (calls_count /= 3) error stop 216
    r = minloc(a, dim = 4, mask = m, back = get_scalar_false())
    if (calls_count /= 4) error stop 218
  end subroutine
end program p
