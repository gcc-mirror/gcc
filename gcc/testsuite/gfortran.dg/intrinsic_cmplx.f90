! { dg-do compile }
! PR fortran/40727
program test
  integer, parameter :: sp = kind(1.e0), dp = kind(1.d0)
  complex(sp) :: s
  complex(dp) :: d
  s =  cmplx(0.e0, cmplx(0.e0,0.e0)) ! { dg-error "either REAL or INTEGER" }
  d = dcmplx(0.d0, cmplx(0.d0,0.d0)) ! { dg-error "either REAL or INTEGER" }
end program test
