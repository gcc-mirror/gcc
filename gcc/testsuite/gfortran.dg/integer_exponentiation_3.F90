! { dg-options "" }
! { dg-options "-ffloat-store" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } 
!
!
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

  interface acheck
    module procedure acheck_c8
    module procedure acheck_c4
  end interface acheck

contains

  subroutine check_i8 (a, b)
    integer(kind=8), intent(in) :: a, b
    if (a /= b) STOP 1
  end subroutine check_i8

  subroutine check_i4 (a, b)
    integer(kind=4), intent(in) :: a, b
    if (a /= b) STOP 2
  end subroutine check_i4

  subroutine check_r8 (a, b)
    real(kind=8), intent(in) :: a, b
    if (a /= b) STOP 3
  end subroutine check_r8

  subroutine check_r4 (a, b)
    real(kind=4), intent(in) :: a, b
    if (a /= b) STOP 4
  end subroutine check_r4

  subroutine check_c8 (a, b)
    complex(kind=8), intent(in) :: a, b
    if (a /= b) STOP 5
  end subroutine check_c8

  subroutine check_c4 (a, b)
    complex(kind=4), intent(in) :: a, b
    if (a /= b) STOP 6
  end subroutine check_c4

  subroutine acheck_c8 (a, b)
    complex(kind=8), intent(in) :: a, b
    if (abs(a-b) > 1.d-9 * min(abs(a),abs(b))) STOP 7
  end subroutine acheck_c8

  subroutine acheck_c4 (a, b)
    complex(kind=4), intent(in) :: a, b
    if (abs(a-b) > 1.e-5 * min(abs(a),abs(b))) STOP 8
  end subroutine acheck_c4

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
#define ATEST(base,exp,var) var = base; call acheck((var)**(exp),(base)**(exp))

!!!!! INTEGER BASE !!!!!
  TEST(0,0,i4)
  TEST(0_8,0_8,i8)
  TEST(1,0,i4)
  TEST(1_8,0_8,i8)
  TEST(-1,0,i4)
  TEST(-1_8,0_8,i8)
  TEST(huge(0_4),0,i4)
  TEST(huge(0_8),0_8,i8)
  TEST(-huge(0_4)-1,0,i4)
  TEST(-huge(0_8)-1_8,0_8,i8)

  TEST(1,1,i4)
  TEST(1_8,1_8,i8)
  TEST(1,2,i4)
  TEST(1_8,2_8,i8)
  TEST(1,-1,i4)
  TEST(1_8,-1_8,i8)
  TEST(1,-2,i4)
  TEST(1_8,-2_8,i8)
  TEST(1,huge(0),i4)
  TEST(1_8,huge(0_8),i8)
  TEST(1,-huge(0)-1,i4)
  TEST(1_8,-huge(0_8)-1_8,i8)

  TEST(-1,1,i4)
  TEST(-1_8,1_8,i8)
  TEST(-1,2,i4)
  TEST(-1_8,2_8,i8)
  TEST(-1,-1,i4)
  TEST(-1_8,-1_8,i8)
  TEST(-1,-2,i4)
  TEST(-1_8,-2_8,i8)
  TEST(-1,huge(0),i4)
  TEST(-1_8,huge(0_8),i8)
  TEST(-1,-huge(0)-1,i4)
  TEST(-1_8,-huge(0_8)-1_8,i8)

  TEST(2,9,i4)
  TEST(2_8,9_8,i8)
  TEST(-2,9,i4)
  TEST(-2_8,9_8,i8)
  TEST(2,-9,i4)
  TEST(2_8,-9_8,i8)
  TEST(-2,-9,i4)
  TEST(-2_8,-9_8,i8)

!!!!! REAL BASE !!!!!
  TEST(0.0,0,r4)
  TEST(0.0,1,r4)
  TEST(0.0,huge(0),r4)
  TEST(0.0,0_8,r4)
  TEST(0.0,1_8,r4)
  TEST(0.0,huge(0_8),r4)

  TEST(1.0,0,r4)
  TEST(1.0,1,r4)
  TEST(1.0,-1,r4)
  TEST(1.0,huge(0),r4)
  TEST(1.0,-huge(0)-1,r4)
  TEST(1.0,0_8,r4)
  TEST(1.0,1_8,r4)
  TEST(1.0,-1_8,r4)
  TEST(1.0,huge(0_8),r4)
  TEST(1.0,-huge(0_8)-1_8,r4)

  TEST(-1.0,0,r4)
  TEST(-1.0,1,r4)
  TEST(-1.0,-1,r4)
  TEST(-1.0,huge(0),r4)
  TEST(-1.0,-huge(0)-1,r4)
  TEST(-1.0,0_8,r4)
  TEST(-1.0,1_8,r4)
  TEST(-1.0,-1_8,r4)
  TEST(-1.0,huge(0_8),r4)
  TEST(-1.0,-huge(0_8)-1_8,r4)

  TEST(2.0,0,r4)
  TEST(2.0,1,r4)
  TEST(2.0,-1,r4)
  TEST(2.0,3,r4)
  TEST(2.0,-3,r4)
  TEST(2.0,0_8,r4)
  TEST(2.0,1_8,r4)
  TEST(2.0,-1_8,r4)
  TEST(2.0,3_8,r4)
  TEST(2.0,-3_8,r4)

  TEST(nearest(1.0,-1.0),0,r4)
  TEST(nearest(1.0,-1.0),huge(0_4),r4) ! { dg-warning "Arithmetic underflow" }
  TEST(nearest(1.0,-1.0),0_8,r4)
  TEST(nearest(1.0_8,-1.0),huge(0_8),r8) ! { dg-warning "Arithmetic underflow" }

  TEST(nearest(1.0,-1.0),107,r4)
  TEST(nearest(1.0,1.0),107,r4)

!!!!! COMPLEX BASE !!!!!
  TEST((1.0,0.2),0,c4)
  TEST((1.0,0.2),1,c4)
  TEST((1.0,0.2),2,c4)
  ATEST((1.0,0.2),9,c4)
  ATEST((1.0,0.2),-1,c4)
  ATEST((1.0,0.2),-2,c4)
  ATEST((1.0,0.2),-9,c4)

  TEST((0.0,0.2),0,c4)
  TEST((0.0,0.2),1,c4)
  TEST((0.0,0.2),2,c4)
  ATEST((0.0,0.2),9,c4)
  ATEST((0.0,0.2),-1,c4)
  ATEST((0.0,0.2),-2,c4)
  ATEST((0.0,0.2),-9,c4)

  TEST((1.0,0.),0,c4)
  TEST((1.0,0.),1,c4)
  TEST((1.0,0.),2,c4)
  TEST((1.0,0.),9,c4)
  ATEST((1.0,0.),-1,c4)
  ATEST((1.0,0.),-2,c4)
  ATEST((1.0,0.),-9,c4)

end program test
