! { dg-do run }

program test
  implicit none

  interface check
    procedure check_r4
    procedure check_r8
  end interface check

  real(kind=4) :: x4
  real(kind=8) :: x8

  x8 = 1.9_8 ; x4 = 1.9_4
  call check(bessel_j0 (x8), bessel_j0 (1.9_8))
  call check(bessel_j0 (x4), bessel_j0 (1.9_4))
  call check(bessel_j1 (x8), bessel_j1 (1.9_8))
  call check(bessel_j1 (x4), bessel_j1 (1.9_4))
  call check(bessel_jn (3,x8), bessel_jn (3,1.9_8))
  call check(bessel_jn (3,x4), bessel_jn (3,1.9_4))
  call check(bessel_y0 (x8), bessel_y0 (1.9_8))
  call check(bessel_y0 (x4), bessel_y0 (1.9_4))
  call check(bessel_y1 (x8), bessel_y1 (1.9_8))
  call check(bessel_y1 (x4), bessel_y1 (1.9_4))
  call check(bessel_yn (3,x8), bessel_yn (3,1.9_8))
  call check(bessel_yn (3,x4), bessel_yn (3,1.9_4))

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
