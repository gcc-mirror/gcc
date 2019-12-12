! { dg-do run }
! { dg-options "-fno-range-check -pedantic" }
! { dg-add-options ieee }
!
! PR fortran/34333
!
! Check that (NaN /= NaN) == .TRUE.
! and some other NaN options.
!
! Contrary to nan_1.f90, PARAMETERs are used and thus
! the front end resolves the min, max and binary operators at
! compile time.
!

module aux2
  interface isinf
    module procedure isinf_r
    module procedure isinf_d
  end interface isinf
contains
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
  real, parameter :: nan = 0.0/0.0, large = huge(large), inf = 1.0/0.0

  if (nan == nan .or. nan > nan .or. nan < nan .or. nan >= nan &
      .or. nan <= nan) STOP 1
  if (isnan (2.d0) .or. (.not. isnan(nan)) .or. &
      (.not. isnan(real(nan,kind=kind(2.d0))))) STOP 2

  ! Create an INF and check it
  if (isinf(nan) .or. isinf(large) .or. .not. isinf(inf)) STOP 3
  if (isinf(-nan) .or. isinf(-large) .or. .not. isinf(-inf)) STOP 4

  ! Check that MIN and MAX behave correctly
  if (max(2.0, nan) /= 2.0) STOP 5
  if (min(2.0, nan) /= 2.0) STOP 6
  if (max(nan, 2.0) /= 2.0) STOP 7
  if (min(nan, 2.0) /= 2.0) STOP 8

  if (max(2.d0, nan) /= 2.d0) STOP 9! { dg-warning "Extension: Different type kinds" }
  if (min(2.d0, nan) /= 2.d0) STOP 10! { dg-warning "Extension: Different type kinds" }
  if (max(nan, 2.d0) /= 2.d0) STOP 11! { dg-warning "Extension: Different type kinds" }
  if (min(nan, 2.d0) /= 2.d0) STOP 12! { dg-warning "Extension: Different type kinds" }

  if (.not. isnan(min(nan,nan))) STOP 13
  if (.not. isnan(max(nan,nan))) STOP 14

  ! Same thing, with more arguments

  if (max(3.0, 2.0, nan) /= 3.0) STOP 15
  if (min(3.0, 2.0, nan) /= 2.0) STOP 16
  if (max(3.0, nan, 2.0) /= 3.0) STOP 17
  if (min(3.0, nan, 2.0) /= 2.0) STOP 18
  if (max(nan, 3.0, 2.0) /= 3.0) STOP 19
  if (min(nan, 3.0, 2.0) /= 2.0) STOP 20

  if (max(3.d0, 2.d0, nan) /= 3.d0) STOP 21! { dg-warning "Extension: Different type kinds" }
  if (min(3.d0, 2.d0, nan) /= 2.d0) STOP 22! { dg-warning "Extension: Different type kinds" }
  if (max(3.d0, nan, 2.d0) /= 3.d0) STOP 23! { dg-warning "Extension: Different type kinds" }
  if (min(3.d0, nan, 2.d0) /= 2.d0) STOP 24! { dg-warning "Extension: Different type kinds" }
  if (max(nan, 3.d0, 2.d0) /= 3.d0) STOP 25! { dg-warning "Extension: Different type kinds" }
  if (min(nan, 3.d0, 2.d0) /= 2.d0) STOP 26! { dg-warning "Extension: Different type kinds" }

  if (.not. isnan(min(nan,nan,nan))) STOP 27
  if (.not. isnan(max(nan,nan,nan))) STOP 28
  if (.not. isnan(min(nan,nan,nan,nan))) STOP 29
  if (.not. isnan(max(nan,nan,nan,nan))) STOP 30
  if (.not. isnan(min(nan,nan,nan,nan,nan))) STOP 31
  if (.not. isnan(max(nan,nan,nan,nan,nan))) STOP 32

  ! Large values, INF and NaNs
  if (.not. isinf(max(large, inf))) STOP 33
  if (isinf(min(large, inf))) STOP 34
  if (.not. isinf(max(nan, large, inf))) STOP 35
  if (isinf(min(nan, large, inf))) STOP 36
  if (.not. isinf(max(large, nan, inf))) STOP 37
  if (isinf(min(large, nan, inf))) STOP 38
  if (.not. isinf(max(large, inf, nan))) STOP 39
  if (isinf(min(large, inf, nan))) STOP 40

  if (.not. isinf(min(-large, -inf))) STOP 41
  if (isinf(max(-large, -inf))) STOP 42
  if (.not. isinf(min(nan, -large, -inf))) STOP 43
  if (isinf(max(nan, -large, -inf))) STOP 44
  if (.not. isinf(min(-large, nan, -inf))) STOP 45
  if (isinf(max(-large, nan, -inf))) STOP 46
  if (.not. isinf(min(-large, -inf, nan))) STOP 47
  if (isinf(max(-large, -inf, nan))) STOP 48

end program test
