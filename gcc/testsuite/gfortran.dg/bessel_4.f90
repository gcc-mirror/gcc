! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/36158 - Transformational BESSEL_JN/YN
! PR fortran/33197 - F2008 math functions
!
implicit none
! OK, elemental function:
 print *, bessel_yn(1, [1.0, 2.0])
 print *, bessel_yn([1, 2], 2.0)

! Wrong, transformational function:
! Does not pass check.c -- thus regarded as wrong generic function
! and thus rejected with a slightly misleading error message
 print *, bessel_yn(1, 2, [2.0, 3.0]) ! { dg-error "Too many arguments" }

! Wrong in F2008: Negative argument, ok as GNU extension
 print *, bessel_yn(-1, 3.0) ! { dg-error "Extension: Negative argument N " }

! Wrong in F2008: Negative argument -- and no need for a GNU extension
! Does not pass check.c -- thus regarded as wrong generic function
! and thus rejected with a slightly misleading error message
 print *, bessel_yn(-1, 2, 3.0) ! { dg-error "Too many arguments" }
end
