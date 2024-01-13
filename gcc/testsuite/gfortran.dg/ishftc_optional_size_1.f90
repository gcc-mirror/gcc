! { dg-do run }
!
! PR fortran/67277 - ISHFTC and missing optional argument SIZE

module m
  implicit none
contains
  ! Optional argument passed by reference
  elemental function ishftc4_ref (i, shift, size_) result(r)
    integer(4), intent(in)           :: i
    integer,    intent(in)           :: shift
    integer,    intent(in), optional :: size_
    integer                          :: r
    r = ishftc (i, shift=shift, size=size_)
  end

  elemental function ishftc1_ref (i, shift, size_) result(r)
    integer(1), intent(in)           :: i
    integer,    intent(in)           :: shift
    integer(1), intent(in), optional :: size_
    integer(1)                       :: r
    r = ishftc (i, shift=shift, size=size_)
  end

  ! Array valued argument i
  function ishftc4_ref_4 (i, shift, size_) result(r)
    integer(4), intent(in)           :: i(4)
    integer,    intent(in)           :: shift
    integer,    intent(in), optional :: size_
    integer                          :: r(size(i))
    r = ishftc (i, shift=shift, size=size_)
  end

  ! Optional argument passed by value
  elemental function ishftc4_val (i, shift, size_) result(r)
    integer(4), intent(in)           :: i
    integer,    intent(in)           :: shift
    integer,    value,      optional :: size_
    integer                          :: r
    r = ishftc (i, shift=shift, size=size_)
  end

  elemental function ishftc1_val (i, shift, size_) result(r)
    integer(1), intent(in)           :: i
    integer,    intent(in)           :: shift
    integer(1), value,      optional :: size_
    integer(1)                       :: r
    r = ishftc (i, shift=shift, size=size_)
  end

  ! Array valued argument i
  function ishftc4_val_4 (i, shift, size_) result(r)
    integer(4), intent(in)           :: i(4)
    integer,    intent(in)           :: shift
    integer,    value,      optional :: size_
    integer                          :: r(size(i))
    r = ishftc (i, shift=shift, size=size_)
  end
end module m

program p
  use m
  implicit none
  integer    :: shift = 1
  integer(4) :: i4 = 127, j4(4), k4(4)
  integer(1) :: i1 = 127
  integer(4) :: expect4
  integer(1) :: expect1

  ! Scalar variants
  expect4 = 2*i4
  if (ishftc      (i4, shift) /= expect4) stop 1
  if (ishftc4_ref (i4, shift) /= expect4) stop 2
  if (ishftc4_val (i4, shift) /= expect4) stop 3

  expect1 = -2_1
  if (ishftc      (i1, shift) /= expect1) stop 4
  if (ishftc1_ref (i1, shift) /= expect1) stop 5
  if (ishftc1_val (i1, shift) /= expect1) stop 6

  ! Array arguments
  expect4 = 2*i4
  j4 = i4
  k4 = ishftc        (j4, shift)
  if (any (k4 /= expect4)) stop 7

  ! The following works on x86_64 but might currently fail on other systems:
  ! (see PR113377)
! k4 = ishftc4_val_4 (j4, shift)
! if (any (k4 /= expect4)) stop 8

  ! The following currently segfaults (might be a scalarizer issue):
  ! (see PR113377)
! k4 = ishftc4_ref_4 (j4, shift)
! print *, k4
! if (any (k4 /= expect4)) stop 9
end program p
