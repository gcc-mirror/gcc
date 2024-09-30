! { dg-do run }
!
! Check that the inline implementation of MAXLOC correctly supports indirect
! MASK, that is a MASK expression that is itself an optional variable.

program p
  implicit none
  integer, parameter :: data(*) = (/ 3, 7, 1, 0, 7, 0, 3, 5, 3, 0 /)
  logical, parameter :: mask(*) = (/ .true. , .false., .true., .true. , &
                                   & .false., .true. , .true., .false., &
                                   & .true. , .true. /)
  call check_int_const_shape_absent_back
  call check_int_const_shape_false_back
  call check_int_const_shape_true_back
  call check_int_const_shape_scalar_mask_absent_back
  call check_int_const_shape_scalar_mask_false_back
  call check_int_const_shape_scalar_mask_true_back
  call check_int_assumed_shape_absent_back
  call check_int_assumed_shape_false_back
  call check_int_assumed_shape_true_back
  call check_int_assumed_shape_scalar_mask_absent_back
  call check_int_assumed_shape_scalar_mask_false_back
  call check_int_assumed_shape_scalar_mask_true_back
  call check_int_func_absent_back
  call check_int_func_false_back
  call check_int_func_true_back
  call check_int_func_scalar_mask_absent_back
  call check_int_func_scalar_mask_false_back
  call check_int_func_scalar_mask_true_back
  call check_int_const_shape_array_mask_absent_back
  call check_int_const_shape_array_mask_false_back
  call check_int_const_shape_array_mask_true_back
  call check_int_assumed_shape_array_mask_absent_back
  call check_int_assumed_shape_array_mask_false_back
  call check_int_assumed_shape_array_mask_true_back
  call check_real_const_shape_absent_back
  call check_real_const_shape_false_back
  call check_real_const_shape_true_back
  call check_real_const_shape_scalar_mask_absent_back
  call check_real_const_shape_scalar_mask_false_back
  call check_real_const_shape_scalar_mask_true_back
  call check_real_assumed_shape_absent_back
  call check_real_assumed_shape_false_back
  call check_real_assumed_shape_true_back
  call check_real_assumed_shape_scalar_mask_absent_back
  call check_real_assumed_shape_scalar_mask_false_back
  call check_real_assumed_shape_scalar_mask_true_back
contains
  subroutine call_maxloc_int_const_shape(r, a, b)
    integer :: r, a(10)
    logical, optional :: b
    r = maxloc(a, dim = 1, back = b)
  end subroutine
  subroutine check_int_const_shape_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape(r, a)
    if (r /= 2) stop 9
  end subroutine
  subroutine check_int_const_shape_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape(r, a, .false.)
    if (r /= 2) stop 16
  end subroutine
  subroutine check_int_const_shape_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape(r, a, .true.)
    if (r /= 5) stop 23
  end subroutine
  subroutine call_maxloc_int_const_shape_scalar_mask(r, a, m, b)
    integer :: r, a(10)
    logical :: m
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_const_shape_scalar_mask_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape_scalar_mask(r, a, .true.)
    if (r /= 2) stop 30
  end subroutine
  subroutine check_int_const_shape_scalar_mask_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape_scalar_mask(r, a, .true., .false.)
    if (r /= 2) stop 37
  end subroutine
  subroutine check_int_const_shape_scalar_mask_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_const_shape_scalar_mask(r, a, .true., .true.)
    if (r /= 5) stop 44
  end subroutine
  subroutine call_maxloc_int_assumed_shape(r, a, b)
    integer :: r, a(:)
    logical, optional :: b
    r = maxloc(a, dim = 1, back = b)
  end subroutine
  subroutine check_int_assumed_shape_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape(r, a)
    if (r /= 2) stop 51
  end subroutine
  subroutine check_int_assumed_shape_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape(r, a, .false.)
    if (r /= 2) stop 58
  end subroutine
  subroutine check_int_assumed_shape_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape(r, a, .true.)
    if (r /= 5) stop 65
  end subroutine
  subroutine call_maxloc_int_assumed_shape_scalar_mask(r, a, m, b)
    integer :: r, a(:)
    logical :: m
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_assumed_shape_scalar_mask_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape_scalar_mask(r, a, .true.)
    if (r /= 2) stop 72
  end subroutine
  subroutine check_int_assumed_shape_scalar_mask_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape_scalar_mask(r, a, .true., .false.)
    if (r /= 2) stop 79
  end subroutine
  subroutine check_int_assumed_shape_scalar_mask_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_assumed_shape_scalar_mask(r, a, .true., .true.)
    if (r /= 5) stop 86
  end subroutine
  function id(a) result(r)
    integer, dimension(:) :: a
    integer, dimension(size(a, dim = 1)) :: r
    r = a
  end function
  subroutine call_maxloc_int_func(r, a, b)
    integer :: r, a(:)
    logical, optional :: b
    r = maxloc(id(a) + 1, dim = 1, back = b)
  end subroutine
  subroutine check_int_func_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func(r, a)
    if (r /= 2) stop 93
  end subroutine
  subroutine check_int_func_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func(r, a, .false.)
    if (r /= 2) stop 100
  end subroutine
  subroutine check_int_func_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func(r, a, .true.)
    if (r /= 5) stop 107
  end subroutine
  subroutine call_maxloc_int_func_scalar_mask(r, a, m, b)
    integer :: r, a(:)
    logical :: m
    logical, optional :: b
    r = maxloc(id(a) + 1, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_func_scalar_mask_absent_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func_scalar_mask(r, a, .true.)
    if (r /= 2) stop 114
  end subroutine
  subroutine check_int_func_scalar_mask_false_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func_scalar_mask(r, a, .true., .false.)
    if (r /= 2) stop 121
  end subroutine
  subroutine check_int_func_scalar_mask_true_back
    integer :: r, a(10)
    a = data
    call call_maxloc_int_func_scalar_mask(r, a, .true., .true.)
    if (r /= 5) stop 128
  end subroutine
  subroutine call_maxloc_int_const_shape_array_mask(r, a, m, b)
    integer :: r, a(10)
    logical :: m(10)
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_const_shape_array_mask_absent_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_const_shape_array_mask(r, a, m)
    if (r /= 1) stop 135
  end subroutine
  subroutine check_int_const_shape_array_mask_false_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_const_shape_array_mask(r, a, m, .false.)
    if (r /= 1) stop 142
  end subroutine
  subroutine check_int_const_shape_array_mask_true_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_const_shape_array_mask(r, a, m, .true.)
    if (r /= 9) stop 149
  end subroutine
  subroutine call_maxloc_int_assumed_shape_array_mask(r, a, m, b)
    integer :: r, a(:)
    logical :: m(:)
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_int_assumed_shape_array_mask_absent_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_assumed_shape_array_mask(r, a, m)
    if (r /= 1) stop 156
  end subroutine
  subroutine check_int_assumed_shape_array_mask_false_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_assumed_shape_array_mask(r, a, m, .false.)
    if (r /= 1) stop 163
  end subroutine
  subroutine check_int_assumed_shape_array_mask_true_back
    integer :: r, a(10)
    logical :: m(10)
    a = data
    m = mask
    call call_maxloc_int_assumed_shape_array_mask(r, a, m, .true.)
    if (r /= 9) stop 170
  end subroutine
  subroutine call_maxloc_real_const_shape(r, a, b)
    integer :: r
    real :: a(10)
    logical, optional :: b
    r = maxloc(a, dim = 1, back = b)
  end subroutine
  subroutine check_real_const_shape_absent_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape(r, a)
    if (r /= 2) stop 177
  end subroutine
  subroutine check_real_const_shape_false_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape(r, a, .false.)
    if (r /= 2) stop 184
  end subroutine
  subroutine check_real_const_shape_true_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape(r, a, .true.)
    if (r /= 5) stop 191
  end subroutine
  subroutine call_maxloc_real_const_shape_scalar_mask(r, a, m, b)
    integer :: r
    real :: a(10)
    logical :: m
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_real_const_shape_scalar_mask_absent_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape_scalar_mask(r, a, .true.)
    if (r /= 2) stop 198
  end subroutine
  subroutine check_real_const_shape_scalar_mask_false_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape_scalar_mask(r, a, .true., .false.)
    if (r /= 2) stop 205
  end subroutine
  subroutine check_real_const_shape_scalar_mask_true_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_const_shape_scalar_mask(r, a, .true., .true.)
    if (r /= 5) stop 212
  end subroutine
  subroutine call_maxloc_real_assumed_shape(r, a, b)
    integer :: r
    real :: a(:)
    logical, optional :: b
    r = maxloc(a, dim = 1, back = b)
  end subroutine
  subroutine check_real_assumed_shape_absent_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape(r, a)
    if (r /= 2) stop 219
  end subroutine
  subroutine check_real_assumed_shape_false_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape(r, a, .false.)
    if (r /= 2) stop 226
  end subroutine
  subroutine check_real_assumed_shape_true_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape(r, a, .true.)
    if (r /= 5) stop 233
  end subroutine
  subroutine call_maxloc_real_assumed_shape_scalar_mask(r, a, m, b)
    integer :: r
    real :: a(:)
    logical :: m
    logical, optional :: b
    r = maxloc(a, dim = 1, mask = m, back = b)
  end subroutine
  subroutine check_real_assumed_shape_scalar_mask_absent_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape_scalar_mask(r, a, .true.)
    if (r /= 2) stop 240
  end subroutine
  subroutine check_real_assumed_shape_scalar_mask_false_back
    integer :: r
    real :: a(10)
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape_scalar_mask(r, a, .true., .false.)
    if (r /= 2) stop 247
  end subroutine
  subroutine check_real_assumed_shape_scalar_mask_true_back
    integer :: r
    real :: a(10)
    a = data
    a = (/ real :: data /)
    call call_maxloc_real_assumed_shape_scalar_mask(r, a, .true., .true.)
    if (r /= 5) stop 254
  end subroutine
end program p
