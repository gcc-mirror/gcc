! { dg-do compile }
! { dg-options "-Wconversion -Wconversion-extra" }
! PR 47359 - additional warnings for conversions.
program main
  implicit none
  real(kind=8) :: a,b
  complex(kind=8) :: c
  integer :: i
  real(kind=4) :: r
  a = 0.13              ! { dg-warning "Conversion" }
  print *,0.1_8 ** 0.2  ! { dg-warning "Conversion" }
  b = a/0.13            ! { dg-warning "Conversion" }
  i = 12345.            ! { dg-warning "Conversion" }
  i = (1., 23.)         ! { dg-warning "Non-zero imaginary part" }
  r = (1., 23.)         ! { dg-warning "Non-zero imaginary part" }
  b = 0.&                 ! { dg-warning "Possible change of value" }
       &5_8*c             ! { dg-warning "Conversion" }
  c = 0.3               ! { dg-warning "Conversion" }
  a = 0.5               ! { dg-warning "Conversion" }
end program main
  
