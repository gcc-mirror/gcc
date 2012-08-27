! { dg-do run }
! { dg-options "-Wall -fno-range-check" }
!
! PR fortran/36158 - Transformational BESSEL_JN/YN
! PR fortran/33197 - F2008 math functions
!
! This is a dg-do run test as the middle end cannot simplify the
! the scalarization of the elemental function (cf. PR 45305).
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
  call abort()
end if


! Difference to mpfr_yn <= 4 epsilon

if (any (abs (BESSEL_YN(2, 5, 2.457) - [(BESSEL_YN(i, 2.457), i = 2, 5)]) &
         > epsilon(0.0)*4)) then
  call abort()
end if


! Difference to mpfr_jn <= 1 epsilon

if (any (abs (BESSEL_JN(0, 10, 4.457) &
              - [ (BESSEL_JN(i, 4.457), i = 0, 10) ]) &
         > epsilon(0.0))) then
  call abort()
end if


! Difference to mpfr_yn <= 192 epsilon

if (any (abs (BESSEL_YN(0, 10, 4.457) &
              - [ (BESSEL_YN(i, 4.457), i = 0, 10) ]) &
         > epsilon(0.0)*192)) then
  call abort()
end if


! Difference to mpfr_jn: None.  (Special case: X = 0.0)

if (any (BESSEL_JN(0, 10, 0.0) /= [ (BESSEL_JN(i, 0.0), i = 0, 10) ])) &
then
  call abort()
end if


! Difference to mpfr_yn: None.  (Special case: X = 0.0)

if (any (BESSEL_YN(0, 10, 0.0) /= [ (BESSEL_YN(i, 0.0), i = 0, 10) ])) &
then
  call abort()
end if


! Difference to mpfr_jn <= 1 epsilon

if (any (abs (BESSEL_JN(0, 10, 1.0) &
              - [ (BESSEL_JN(i, 1.0), i = 0, 10) ]) &
         > epsilon(0.0)*1)) then
 call abort()
end if

! Difference to mpfr_yn <= 32 epsilon

if (any (abs (BESSEL_YN(0, 10, 1.0) &
              - [ (BESSEL_YN(i, 1.0), i = 0, 10) ]) &
         > epsilon(0.0)*32)) then
  call abort()
end if

end
