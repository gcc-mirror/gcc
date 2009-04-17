! { dg-do run { xfail spu-*-* } }
! FAILs on SPU because of invalid result of 1.0/0.0 inline code
! { dg-options "-fno-range-check" }
! { dg-options "-fno-range-check -mieee" { target alpha*-*-* } } */
module mod_check
  implicit none

  interface check
    module procedure check_i8
    module procedure check_i4
    module procedure check_r8
    module procedure check_r4
    module procedure check_c8
    module procedure check_c4
  end interface check

contains

  subroutine check_i8 (a, b)
    integer(kind=8), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_i8

  subroutine check_i4 (a, b)
    integer(kind=4), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_i4

  subroutine check_r8 (a, b)
    real(kind=8), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_r8

  subroutine check_r4 (a, b)
    real(kind=4), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_r4

  subroutine check_c8 (a, b)
    complex(kind=8), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_c8

  subroutine check_c4 (a, b)
    complex(kind=4), intent(in) :: a, b
    if (a /= b) call abort()
  end subroutine check_c4

end module mod_check

program test
  use mod_check
  implicit none

  integer(kind=4) :: i4
  integer(kind=8) :: i8
  real(kind=4) :: r4
  real(kind=8) :: r8
  complex(kind=4) :: c4
  complex(kind=8) :: c8

#define TEST(base,exp,var) var = base; call check((var)**(exp),(base)**(exp))

!!!!! INTEGER BASE !!!!!
  TEST(3,23,i4)
  TEST(-3,23,i4)
  TEST(3_8,43_8,i8)
  TEST(-3_8,43_8,i8)

  TEST(17_8,int(huge(0_4),kind=8)+1,i8)

!!!!! REAL BASE !!!!!
  TEST(0.0,-1,r4)
  TEST(0.0,-huge(0)-1,r4)
  TEST(2.0,huge(0),r4)
  TEST(nearest(1.0,-1.0),-huge(0),r4)

end program test

! { dg-final { cleanup-modules "mod_check" } }
