! { dg-do compile }
! { dg-options "-Wconversion-extra -Wconversion" }
program main
  implicit none
  double precision, parameter :: pi = &  ! { dg-warning "Conversion" }
       & 3.1415829535897932     ! { dg-warning "Non-significant digits" }
end program main
