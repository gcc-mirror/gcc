! { dg-do  run }
!
! PR fortran/45424
! PR fortran/48820
!
! Additional run-time checks for IS_CONTIGUOUS with assumed type/rank
program is_contiguous_2
  implicit none
  real, allocatable :: b(:,:)
  real, pointer     :: c(:,:)
  integer, volatile :: k
  target :: b
  allocate(b(10,10))
  k = 2
  if (fail_ar (b,          .true.) ) stop 1
  if (fail_ar (b(::1,::1), .true.) ) stop 2
  if (fail_ar (b(::2,::1), .false.)) stop 3
  if (fail_ar (b(::1,::2), .false.)) stop 4
  if (fail_ar (b(:10,:10), .true. )) stop 5
  if (fail_ar (b(: 9,:10), .false.)) stop 6
  if (fail_ar (b(2: ,:  ), .false.)) stop 7
  if (fail_ar (b(:  ,2: ), .true. )) stop 8
  if (fail_ar (b(k: ,:  ), .false.)) stop 9
  if (fail_ar (b(:  ,k: ), .true. )) stop 10
  if (fail_at (b(::1,k: ), .true. )) stop 11
  if (fail_at (b(::k,k: ), .false.)) stop 12
  if (fail_at (b(10,k)   , .true. )) stop 13
  c => b(::1,:)
  if (fail_ar (c,          .true.) ) stop 14
  c => b(::2,:)
  if (fail_ar (c,          .false.)) stop 15
  associate (d => b(:,2:), e => b(::k,:))
    if (fail_ar (d,        .true.) ) stop 16
    if (fail_ar (e,        .false.)) stop 17
  end associate
contains
  pure logical function fail_ar (x, expect) result (fail)
    real,    dimension(..), intent(in) :: x  ! Assumed rank
    logical,                intent(in) :: expect
    fail = is_contiguous (x) .neqv. expect
  end function fail_ar
  pure logical function fail_at (x, expect) result (fail)
    type(*), dimension(..), intent(in) :: x  ! Assumed type/assumed rank
    logical,                intent(in) :: expect
    fail = is_contiguous (x) .neqv. expect
  end function fail_at
end program
