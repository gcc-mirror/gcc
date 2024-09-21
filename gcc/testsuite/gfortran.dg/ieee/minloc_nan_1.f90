! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline MINLOC implementation,
! when ARRAY is filled with NANs.

program p
  implicit none
  call check_without_mask
  call check_with_mask
contains
  subroutine check_without_mask()
    use, intrinsic :: ieee_arithmetic
    real, allocatable :: a(:,:,:)
    real :: nan
    integer, allocatable :: m(:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(3,3,3), source = nan)
    m = minloc(a)
    if (size(m, dim=1) /= 3) stop 32
    if (any(m /= (/ 1, 1, 1 /))) stop 35
  end subroutine
  subroutine check_with_mask()
    use, intrinsic :: ieee_arithmetic
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    real :: nan
    integer, allocatable :: r(:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(3,3,3), source = nan)
    allocate(m(3,3,3))
    m(:,:,:) = reshape((/ .false., .false., .true. , .true. , .false., &
                          .true. , .false., .false., .false., .true. , &
                          .true. , .false., .true. , .true. , .true. , &
                          .false., .false., .true. , .true. , .false., &
                          .false., .true. , .false., .false., .true. , &
                          .true. , .true. /), shape(m))
    r = minloc(a, mask = m)
    if (size(r, dim = 1) /= 3) stop 62
    if (any(r /= (/ 3, 1, 1 /))) stop 65
  end subroutine
end program p
