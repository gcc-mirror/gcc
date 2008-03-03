! { dg-do run }

program test
  implicit none

  interface check
    procedure check_r4
    procedure check_r8
  end interface check

  real(kind=4) :: x4, y4
  real(kind=8) :: x8, y8

  x8 = 1.9_8 ; x4 = 1.9_4
  y8 = -2.1_8 ; y4 = -2.1_4

  call check(hypot(x8,y8), hypot(1.9_8,-2.1_8))
  call check(hypot(x4,y4), hypot(1.9_4,-2.1_4))

contains
  subroutine check_r4 (a, b)
    real(kind=4), intent(in) :: a, b
    if (abs(a - b) > 1.e-5 * abs(b)) call abort
  end subroutine
  subroutine check_r8 (a, b)
    real(kind=8), intent(in) :: a, b
    if (abs(a - b) > 1.e-7 * abs(b)) call abort
  end subroutine
end program test
