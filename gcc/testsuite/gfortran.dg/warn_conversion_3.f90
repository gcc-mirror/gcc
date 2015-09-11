! { dg-do compile }
! { dg-options "-Wconversion -Wconversion-extra" }
! PR 47659 - warning about conversions on assignment
! Based on a test case by Thomas Henlich
program main
  double precision d1, d2
  complex(8), parameter :: z = cmplx (0.5, 0.5)  ! { dg-warning "Conversion" }
  real :: r1, r2
  r1 = 2.3d0 ! { dg-warning "Change of value in conversion" }
  r2 = 2.5d0 ! { dg-warning "Conversion" }
  d1 = .13 ! { dg-warning "Conversion" }
  d2 = .13d0
  d1 = z     ! { dg-warning "Non-zero imaginary part" }
end program main
