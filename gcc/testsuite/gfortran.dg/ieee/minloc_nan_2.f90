! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline minloc implementation,
! when the dim argument is present.

program p
  implicit none
  call check_without_mask
  call check_with_mask
contains
  subroutine check_without_mask()
    use, intrinsic :: ieee_arithmetic
    real, allocatable :: a(:,:,:)
    real :: nan
    integer, allocatable :: r(:,:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(3,4,5), source = nan)
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) error stop 21
    if (any(r /= 1)) error stop 22
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) error stop 23
    if (any(r /= 1)) error stop 24
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 25
    if (any(r /= 1)) error stop 26
  end subroutine
  subroutine check_with_mask()
    use, intrinsic :: ieee_arithmetic
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    real :: nan
    integer, allocatable :: r(:,:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(2,3,4), source = nan)
    allocate(m(2,3,4))
    m(:,:,:) = reshape((/ .false., .false., .true. , .true. ,  &
                          .false., .true. , .false., .false.,  &
                          .false., .true. , .true. , .false.,  &
                          .true. , .true. , .true. , .false.,  &
                          .false., .true. , .true. , .false.,  &
                          .false., .true. , .false., .false.  /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) error stop 51
    if (any(r /= reshape((/ 0, 1, 2,  &
                            0, 2, 1,  &
                            1, 1, 2,  &
                            1, 2, 0  /), (/ 3, 4 /)))) error stop 52
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 2, 4 /))) error stop 53
    if (any(r /= reshape((/ 2, 2,  &
                            3, 2,  &
                            1, 1,  &
                            1, 2  /), (/ 2, 4 /)))) error stop 54
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 2, 3 /))) error stop 55
    if (any(r /= reshape((/ 3, 3,  &
                            1, 1,  &
                            2, 1  /), (/ 2, 3 /)))) error stop 56
  end subroutine
end program p
