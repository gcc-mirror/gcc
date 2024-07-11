! { dg-do run }
!
! Check that the evaluation of MAXLOC's BACK argument is made only once
! before the scalarisation loops.

program p
  implicit none
  integer, parameter :: data10(*) = (/ 7, 4, 7, 6, 6, 4, 6, 3, 9, 8 /)
  logical, parameter :: mask10(*) = (/ .false., .true., .false., &
                                       .false., .true., .true.,  &
                                       .true. , .true., .false., &
                                       .false. /)
  integer :: calls_count = 0
  call check_int_const_shape
  call check_int_const_shape_scalar_mask
  call check_int_const_shape_array_mask
  call check_int_const_shape_optional_mask_present
  call check_int_const_shape_optional_mask_absent
  call check_int_const_shape_empty
  call check_int_alloc
  call check_int_alloc_scalar_mask
  call check_int_alloc_array_mask
  call check_int_alloc_empty
  call check_real_const_shape
  call check_real_const_shape_scalar_mask
  call check_real_const_shape_array_mask
  call check_real_const_shape_optional_mask_present
  call check_real_const_shape_optional_mask_absent
  call check_real_const_shape_empty
  call check_real_alloc
  call check_real_alloc_scalar_mask
  call check_real_alloc_array_mask
  call check_real_alloc_empty
contains
  function get_scalar_false()
    logical :: get_scalar_false
    calls_count = calls_count + 1
    get_scalar_false = .false.
  end function
  subroutine check_int_const_shape()
    integer :: a(10)
    logical :: m(10)
    integer :: r
    a = data10
    calls_count = 0
    r = maxloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) stop 11
  end subroutine
  subroutine check_int_const_shape_scalar_mask()
    integer :: a(10)
    integer :: r
    a = data10
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by maxloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = maxloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) stop 18
  end subroutine
  subroutine check_int_const_shape_array_mask()
    integer :: a(10)
    logical :: m(10)
    integer :: r
    a = data10
    m = mask10
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 32
  end subroutine
  subroutine call_maxloc_int(r, a, m, b)
    integer :: a(:)
    logical, optional :: m(:)
    logical, optional :: b
    integer :: r
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_const_shape_optional_mask_present()
    integer :: a(10)
    logical :: m(10)
    integer :: r
    a = data10
    m = mask10
    calls_count = 0
    call call_maxloc_int(r, a, m, get_scalar_false())
    if (calls_count /= 1) stop 39
  end subroutine
  subroutine check_int_const_shape_optional_mask_absent()
    integer :: a(10)
    integer :: r
    a = data10
    calls_count = 0
    call call_maxloc_int(r, a, b = get_scalar_false())
    if (calls_count /= 1) stop 46
  end subroutine
  subroutine check_int_const_shape_empty()
    integer :: a(0)
    logical :: m(0)
    integer :: r
    a = (/ integer:: /)
    m = (/ logical:: /)
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 53
  end subroutine
  subroutine check_int_alloc()
    integer, allocatable :: a(:)
    integer :: r
    allocate(a(10))
    a(:) = data10
    calls_count = 0
    r = maxloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) stop 60
  end subroutine
  subroutine check_int_alloc_scalar_mask()
    integer, allocatable :: a(:)
    integer :: r
    allocate(a(10))
    a(:) = data10
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by maxloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = maxloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) stop 67
  end subroutine
  subroutine check_int_alloc_array_mask()
    integer, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer :: r
    allocate(a(10), m(10))
    a(:) = data10
    m(:) = mask10
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 81
  end subroutine
  subroutine check_int_alloc_empty()
    integer, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer :: r
    allocate(a(0), m(0))
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 88
  end subroutine
  subroutine check_real_const_shape()
    real :: a(10)
    integer :: r
    a = (/ real:: data10 /)
    calls_count = 0
    r = maxloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) stop 95
  end subroutine
  subroutine check_real_const_shape_scalar_mask()
    real :: a(10)
    integer :: r
    a = (/ real:: data10 /)
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by maxloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = maxloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) stop 102
  end subroutine
  subroutine check_real_const_shape_array_mask()
    real :: a(10)
    logical :: m(10)
    integer :: r
    a = (/ real:: data10 /)
    m = mask10
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 116
  end subroutine
  subroutine call_maxloc_real(r, a, m, b)
    real :: a(:)
    logical, optional :: m(:)
    logical, optional :: b
    integer :: r
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_real_const_shape_optional_mask_present()
    real :: a(10)
    logical :: m(10)
    integer :: r
    a = (/ real:: data10 /)
    m = mask10
    calls_count = 0
    call call_maxloc_real(r, a, m, b = get_scalar_false())
    if (calls_count /= 1) stop 123
  end subroutine
  subroutine check_real_const_shape_optional_mask_absent()
    real :: a(10)
    integer :: r
    a = (/ real:: data10 /)
    calls_count = 0
    call call_maxloc_real(r, a, b = get_scalar_false())
    if (calls_count /= 1) stop 130
  end subroutine
  subroutine check_real_const_shape_empty()
    real :: a(0)
    logical :: m(0)
    integer :: r
    a = (/ real:: /)
    m = (/ logical:: /)
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 137
  end subroutine
  subroutine check_real_alloc()
    real, allocatable :: a(:)
    integer :: r
    allocate(a(10))
    a(:) = (/ real:: data10 /)
    calls_count = 0
    r = maxloc(a, dim = 1, back = get_scalar_false())
    if (calls_count /= 1) stop 144
  end subroutine
  subroutine check_real_alloc_scalar_mask()
    real, allocatable :: a(:)
    integer :: r
    allocate(a(10))
    a(:) = (/ real:: data10 /)
    calls_count = 0
    ! We only check the case of a .true. mask.
    ! If the mask is .false., the back argument is not necessary to deduce
    ! the value returned by maxloc, so the compiler is free to elide it,
    ! and the value of calls_count is undefined in that case.
    r = maxloc(a, dim = 1, mask = .true., back = get_scalar_false())
    if (calls_count /= 1) stop 151
  end subroutine
  subroutine check_real_alloc_array_mask()
    real, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer :: r
    allocate(a(10), m(10))
    a(:) = (/ real:: data10 /)
    m(:) = mask10
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 165
  end subroutine
  subroutine check_real_alloc_empty()
    real, allocatable :: a(:)
    logical, allocatable :: m(:)
    integer :: r
    allocate(a(0), m(0))
    a(:) = (/ real:: /)
    m(:) = (/ logical :: /)
    calls_count = 0
    r = maxloc(a, dim = 1, mask = m, back = get_scalar_false())
    if (calls_count /= 1) stop 172
  end subroutine
end program p
