! { dg-do compile }
! { dg-options "-Wall" }
!
! Check fix for PR94246 in which the errors in line 63 caused a segfault
! because the cleanup was not done correctly without the -fno-range-check option.
!
! This is a copy of bessel_5.f90 with the error messages added.
!
! -Wall has been specified to disabled -pedantic, which warns about the
! negative order (GNU extension) to the order of the Bessel functions of
! first and second kind.
!

implicit none
integer :: i


! Difference to mpfr_jn <= 1 epsilon

if (any (abs (BESSEL_JN(2, 5, 2.457) - [(BESSEL_JN(i, 2.457), i = 2, 5)]) &
          > epsilon(0.0))) then
  print *, 'FAIL 1'
  STOP 1
end if


! Difference to mpfr_yn <= 4 epsilon

if (any (abs (BESSEL_YN(2, 5, 2.457) - [(BESSEL_YN(i, 2.457), i = 2, 5)]) &
         > epsilon(0.0)*4)) then
  STOP 2
end if


! Difference to mpfr_jn <= 1 epsilon

if (any (abs (BESSEL_JN(0, 10, 4.457) &
              - [ (BESSEL_JN(i, 4.457), i = 0, 10) ]) &
         > epsilon(0.0))) then
  STOP 3
end if


! Difference to mpfr_yn <= 192 epsilon

if (any (abs (BESSEL_YN(0, 10, 4.457) &
              - [ (BESSEL_YN(i, 4.457), i = 0, 10) ]) &
         > epsilon(0.0)*192)) then
  STOP 4
end if


! Difference to mpfr_jn: None.  (Special case: X = 0.0)

if (any (BESSEL_JN(0, 10, 0.0) /= [ (BESSEL_JN(i, 0.0), i = 0, 10) ])) &
then
  STOP 5
end if


! Difference to mpfr_yn: None.  (Special case: X = 0.0)

if (any (BESSEL_YN(0, 10, 0.0) /= [ (BESSEL_YN(i, 0.0), i = 0, 10) ])) & ! { dg-error "overflows|-INF" }
then
  STOP 6
end if


! Difference to mpfr_jn <= 1 epsilon

if (any (abs (BESSEL_JN(0, 10, 1.0) &
              - [ (BESSEL_JN(i, 1.0), i = 0, 10) ]) &
         > epsilon(0.0)*1)) then
 STOP 7
end if

! Difference to mpfr_yn <= 32 epsilon

if (any (abs (BESSEL_YN(0, 10, 1.0) &
              - [ (BESSEL_YN(i, 1.0), i = 0, 10) ]) &
         > epsilon(0.0)*32)) then
  STOP 8
end if

end
