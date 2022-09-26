! { dg-do run }

  use, intrinsic :: ieee_arithmetic
  implicit none

  real :: sx1, sx2, sx3
  double precision :: dx1, dx2, dx3

  ! IEEE_AWAY was added in Fortran 2018 and not supported by any target
  ! at the moment. Just check we can query for its support.

  ! We should support at least C float and C double types
  if (ieee_support_rounding(ieee_away) &
      .or. ieee_support_rounding(ieee_away, 0.) &
      .or. ieee_support_rounding(ieee_away, 0.d0)) then
    print *, "If a target / libc now supports this, we need to add a proper check!"
    stop 1
  end if

end
