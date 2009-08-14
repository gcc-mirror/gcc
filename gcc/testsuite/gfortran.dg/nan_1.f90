! Test if MIN and MAX intrinsics behave correctly when passed NaNs
! as arguments
!
! { dg-do run }
! { dg-options "-pedantic-errors -mieee" { target alpha*-*-* sh*-*-* } } 
!
module aux2
  interface isnan
    module procedure isnan_r
    module procedure isnan_d
  end interface isnan

  interface isinf
    module procedure isinf_r
    module procedure isinf_d
  end interface isinf
contains

  pure function isnan_r(x) result (isnan)
    logical :: isnan
    real, intent(in) :: x

    isnan = (.not.(x == x))
  end function isnan_r

  pure function isnan_d(x) result (isnan)
    logical :: isnan
    double precision, intent(in) :: x

    isnan = (.not.(x == x))
  end function isnan_d

  pure function isinf_r(x) result (isinf)
    logical :: isinf
    real, intent(in) :: x

    isinf = (x > huge(x)) .or. (x < -huge(x))
  end function isinf_r

  pure function isinf_d(x) result (isinf)
    logical :: isinf
    double precision, intent(in) :: x

    isinf = (x > huge(x)) .or. (x < -huge(x))
  end function isinf_d
end module aux2

program test
  use aux2
  implicit none
  real :: nan, large, inf

  ! Create a NaN and check it
  nan = 0
  nan = nan / nan
  if (nan == nan .or. nan > nan .or. nan < nan .or. nan >= nan &
      .or. nan <= nan) call abort
  if (isnan (2.d0) .or. (.not. isnan(nan)) .or. &
      (.not. isnan(real(nan,kind=kind(2.d0))))) call abort

  ! Create an INF and check it
  large = huge(large)
  inf = 2 * large
  if (isinf(nan) .or. isinf(large) .or. .not. isinf(inf)) call abort
  if (isinf(-nan) .or. isinf(-large) .or. .not. isinf(-inf)) call abort

  ! Check that MIN and MAX behave correctly
  if (max(2.0, nan) /= 2.0) call abort
  if (min(2.0, nan) /= 2.0) call abort
  if (max(nan, 2.0) /= 2.0) call abort
  if (min(nan, 2.0) /= 2.0) call abort

  if (max(2.d0, nan) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (min(2.d0, nan) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (max(nan, 2.d0) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (min(nan, 2.d0) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }

  if (.not. isnan(min(nan,nan))) call abort
  if (.not. isnan(max(nan,nan))) call abort

  ! Same thing, with more arguments

  if (max(3.0, 2.0, nan) /= 3.0) call abort
  if (min(3.0, 2.0, nan) /= 2.0) call abort
  if (max(3.0, nan, 2.0) /= 3.0) call abort
  if (min(3.0, nan, 2.0) /= 2.0) call abort
  if (max(nan, 3.0, 2.0) /= 3.0) call abort
  if (min(nan, 3.0, 2.0) /= 2.0) call abort

  if (max(3.d0, 2.d0, nan) /= 3.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (min(3.d0, 2.d0, nan) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (max(3.d0, nan, 2.d0) /= 3.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (min(3.d0, nan, 2.d0) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (max(nan, 3.d0, 2.d0) /= 3.d0) call abort ! { dg-warning "Extension: Different type kinds" }
  if (min(nan, 3.d0, 2.d0) /= 2.d0) call abort ! { dg-warning "Extension: Different type kinds" }

  if (.not. isnan(min(nan,nan,nan))) call abort
  if (.not. isnan(max(nan,nan,nan))) call abort
  if (.not. isnan(min(nan,nan,nan,nan))) call abort
  if (.not. isnan(max(nan,nan,nan,nan))) call abort
  if (.not. isnan(min(nan,nan,nan,nan,nan))) call abort
  if (.not. isnan(max(nan,nan,nan,nan,nan))) call abort

  ! Large values, INF and NaNs
  if (.not. isinf(max(large, inf))) call abort
  if (isinf(min(large, inf))) call abort
  if (.not. isinf(max(nan, large, inf))) call abort
  if (isinf(min(nan, large, inf))) call abort
  if (.not. isinf(max(large, nan, inf))) call abort
  if (isinf(min(large, nan, inf))) call abort
  if (.not. isinf(max(large, inf, nan))) call abort
  if (isinf(min(large, inf, nan))) call abort

  if (.not. isinf(min(-large, -inf))) call abort
  if (isinf(max(-large, -inf))) call abort
  if (.not. isinf(min(nan, -large, -inf))) call abort
  if (isinf(max(nan, -large, -inf))) call abort
  if (.not. isinf(min(-large, nan, -inf))) call abort
  if (isinf(max(-large, nan, -inf))) call abort
  if (.not. isinf(min(-large, -inf, nan))) call abort
  if (isinf(max(-large, -inf, nan))) call abort

end program test

! { dg-final { cleanup-modules "aux2" } }
