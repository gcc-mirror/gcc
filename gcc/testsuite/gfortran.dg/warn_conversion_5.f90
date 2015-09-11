! { dg-do compile }
! { dg-options "-Wconversion" }
! PR 47359 - additional warnings for conversions.
program main
  implicit none
  complex(kind=4) :: c4
  complex(kind=8) :: c8
  real(kind=4) :: r4
  real(kind=8) :: r8
  complex(kind=4), parameter :: c4p = (1.0, -4.)
  complex, parameter :: c8w = (1.0_8, -4.2_8) ! { dg-warning "Change of value in conversion" }
  complex (kind=8), parameter :: c8p = (1.0_8, -4.2_8)
  integer :: i

  c4 = c8p ! { dg-warning "Change of value in conversion" }
  c4 = 2**26 + 1 ! { dg-warning "Change of value in conversion" }
  c4 = 1.3d0     ! { dg-warning "Change of value in conversion" }
  c4 = c8p       ! { dg-warning "Change of value in conversion" }
  c4 = (1.2, 1000000001) ! { dg-warning "Change of value in conversion" }
  r4 = (2**26 + 1) * 2.3 ! { dg-warning "Change of value in conversion" }
  r4 = 2.4d0 ! { dg-warning "Change of value" }
  r4 = c4p ! { dg-warning "Non-zero imaginary part" }
  r4 = r4 + 2.3d0 ! { dg-warning "Possible change of value in conversion" }
  r8 = 2_8**62 - 1_8 ! { dg-warning "Change of value in conversion" }
  i = c4p ! { dg-warning "Non-zero imaginary part" }
  i = 42 + 1.3 ! { dg-warning "Change of value in conversion" }
  i = (1.2, 0.) ! { dg-warning "Change of value in conversion" }
  c4 = 1.2       ! no warning
  c4 = -3.25d0   ! no warning
  c4 = -42       ! no warning
  c8 = 2**26 + 1 ! no warning
  i = 22.        ! no warning
  i = (35., 0.)  ! no warning
  r4 = 2.5d0     ! no warning
  r4 = 235       ! no warning
  r8 = 2.3       ! no warning
end program main
