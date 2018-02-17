! { dg-do run }
!
! { dg-options "" }
! Do not run with -pedantic checks enabled as "check"
! contains internal procedures which is a vendor extension

program test
  implicit none

  interface check
    procedure check_r4
    procedure check_r8
  end interface check

  real(kind=4) :: x4
  real(kind=8) :: x8

  x8 = 1.9_8 ; x4 = 1.9_4

  call check(erfc_scaled(x8), erfc_scaled(1.9_8))
  call check(erfc_scaled(x4), erfc_scaled(1.9_4))

contains
  subroutine check_r4 (a, b)
    real(kind=4), intent(in) :: a, b
    if (abs(a - b) > 1.e-5 * abs(b)) STOP 1
  end subroutine
  subroutine check_r8 (a, b)
    real(kind=8), intent(in) :: a, b
    if (abs(a - b) > 1.e-7 * abs(b)) STOP 2
  end subroutine
end program test
