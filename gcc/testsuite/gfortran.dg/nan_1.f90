! Test if MIN and MAX intrinsics behave correctly when passed NaNs
! as arguments
!
! { dg-do run }
! { dg-add-options ieee }
! { dg-skip-if "NaN not supported" { spu-*-* } }
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
      .or. nan <= nan) STOP 1
  if (isnan (2.d0) .or. (.not. isnan(nan)) .or. &
      (.not. isnan(real(nan,kind=kind(2.d0))))) STOP 2

  ! Create an INF and check it
  large = huge(large)
  inf = 2 * large
  if (isinf(nan) .or. isinf(large) .or. .not. isinf(inf)) STOP 3
  if (isinf(-nan) .or. isinf(-large) .or. .not. isinf(-inf)) STOP 4

  ! Check that MIN and MAX behave correctly

  if (.not. isnan(min(nan,nan))) STOP 13
  if (.not. isnan(max(nan,nan))) STOP 14

  ! Same thing, with more arguments

  if (.not. isnan(min(nan,nan,nan))) STOP 27
  if (.not. isnan(max(nan,nan,nan))) STOP 28
  if (.not. isnan(min(nan,nan,nan,nan))) STOP 29
  if (.not. isnan(max(nan,nan,nan,nan))) STOP 30
  if (.not. isnan(min(nan,nan,nan,nan,nan))) STOP 31
  if (.not. isnan(max(nan,nan,nan,nan,nan))) STOP 32

  ! Large values, INF and NaNs
  if (.not. isinf(max(large, inf))) STOP 33
  if (isinf(min(large, inf))) STOP 34

  if (.not. isinf(min(-large, -inf))) STOP 41
  if (isinf(max(-large, -inf))) STOP 42

end program test
