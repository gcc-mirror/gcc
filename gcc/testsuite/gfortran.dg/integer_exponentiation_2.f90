! { dg-do run }
! { dg-options "" }
! Test various exponentations
! initially designed for patch to PR31120

program test
  call run_me (1.0, 1, (1.0,0.0))
  call run_me (-1.1, -1, (0.0,-1.0))
  call run_me (42.0, 12, (1.0,7.0))
end program test

! This subroutine is for runtime tests
subroutine run_me(a, i, z)
  implicit none

  real, intent(in) :: a
  integer, intent(in) :: i
  complex, intent(in) :: z

  call check_equal_i (i**0, 1)
  call check_equal_i (i**1, i)
  call check_equal_i (i**2, i*i)
  call check_equal_i (i**3, i*(i**2))

  ! i has default integer kind.
  call check_equal_i (int(i**0_8,kind=kind(i)), 1)
  call check_equal_i (int(i**1_8,kind=kind(i)), i)
  call check_equal_i (int(i**2_8,kind=kind(i)), i*i)
  call check_equal_i (int(i**3_8,kind=kind(i)), i*i*i)

  call check_equal_r (a**0.0, 1.0)
  call check_equal_r (a**1.0, a)
  call check_equal_r (a**2.0, a*a)
  call check_equal_r (a**3.0, a*(a**2))
  call check_equal_r (a**(-1.0), 1/a)
  call check_equal_r (a**(-2.0), (1/a)*(1/a))

  call check_equal_r (a**0, 1.0)
  call check_equal_r (a**1, a)
  call check_equal_r (a**2, a*a)
  call check_equal_r (a**3, a*(a**2))
  call check_equal_r (a**(-1), 1/a)
  call check_equal_r (a**(-2), (1/a)*(1/a))

  call check_equal_r (a**0_8, 1.0)
  call check_equal_r (a**1_8, a)
  call check_equal_r (a**2_8, a*a)
  call check_equal_r (a**3_8, a*(a**2))
  call check_equal_r (a**(-1_8), 1/a)
  call check_equal_r (a**(-2_8), (1/a)*(1/a))

  call check_equal_c (z**0.0, (1.0,0.0))
  call check_equal_c (z**1.0, z)
  call check_equal_c (z**2.0, z*z)
  call check_equal_c (z**3.0, z*(z**2))
  call check_equal_c (z**(-1.0), 1/z)
  call check_equal_c (z**(-2.0), (1/z)*(1/z))

  call check_equal_c (z**(0.0,0.0), (1.0,0.0))
  call check_equal_c (z**(1.0,0.0), z)
  call check_equal_c (z**(2.0,0.0), z*z)
  call check_equal_c (z**(3.0,0.0), z*(z**2))
  call check_equal_c (z**(-1.0,0.0), 1/z)
  call check_equal_c (z**(-2.0,0.0), (1/z)*(1/z))

  call check_equal_c (z**0, (1.0,0.0))
  call check_equal_c (z**1, z)
  call check_equal_c (z**2, z*z)
  call check_equal_c (z**3, z*(z**2))
  call check_equal_c (z**(-1), 1/z)
  call check_equal_c (z**(-2), (1/z)*(1/z))

  call check_equal_c (z**0_8, (1.0,0.0))
  call check_equal_c (z**1_8, z)
  call check_equal_c (z**2_8, z*z)
  call check_equal_c (z**3_8, z*(z**2))
  call check_equal_c (z**(-1_8), 1/z)
  call check_equal_c (z**(-2_8), (1/z)*(1/z))


contains

  subroutine check_equal_r (a, b)
    real, intent(in) :: a, b
    if (abs(a - b) > 1.e-5 * abs(b)) STOP 1
  end subroutine check_equal_r

  subroutine check_equal_c (a, b)
    complex, intent(in) :: a, b
    if (abs(a - b) > 1.e-5 * abs(b)) STOP 2
  end subroutine check_equal_c

  subroutine check_equal_i (a, b)
    integer, intent(in) :: a, b
    if (a /= b) STOP 3
  end subroutine check_equal_i

end subroutine run_me

! subroutine foo is used for compilation test only
subroutine foo(a)
  implicit none

  real, intent(in) :: a
  integer :: i
  complex :: z

  ! Integer
  call gee_i(i**0_1)
  call gee_i(i**1_1)
  call gee_i(i**2_1)
  call gee_i(i**3_1)
  call gee_i(i**(-1_1))
  call gee_i(i**(-2_1))
  call gee_i(i**(-3_1))
  call gee_i(i**huge(0_1))
  call gee_i(i**(-huge(0_1)))
  call gee_i(i**(-huge(0_1)-1_1))

  call gee_i(i**0_2)
  call gee_i(i**1_2)
  call gee_i(i**2_2)
  call gee_i(i**3_2)
  call gee_i(i**(-1_2))
  call gee_i(i**(-2_2))
  call gee_i(i**(-3_2))
  call gee_i(i**huge(0_2))
  call gee_i(i**(-huge(0_2)))
  call gee_i(i**(-huge(0_2)-1_2))

  call gee_i(i**0_4)
  call gee_i(i**1_4)
  call gee_i(i**2_4)
  call gee_i(i**3_4)
  call gee_i(i**(-1_4))
  call gee_i(i**(-2_4))
  call gee_i(i**(-3_4))
  call gee_i(i**huge(0_4))
  call gee_i(i**(-huge(0_4)))
  call gee_i(i**(-huge(0_4)-1_4))

  call gee_i8(i**0_8)
  call gee_i8(i**1_8)
  call gee_i8(i**2_8)
  call gee_i8(i**3_8)
  call gee_i8(i**(-1_8))
  call gee_i8(i**(-2_8))
  call gee_i8(i**(-3_8))
  call gee_i8(i**huge(0_8))
  call gee_i8(i**(-huge(0_8)))
  call gee_i8(i**(-huge(0_8)-1_8))

  ! Real
  call gee_r(a**0_1)
  call gee_r(a**1_1)
  call gee_r(a**2_1)
  call gee_r(a**3_1)
  call gee_r(a**(-1_1))
  call gee_r(a**(-2_1))
  call gee_r(a**(-3_1))
  call gee_r(a**huge(0_1))
  call gee_r(a**(-huge(0_1)))
  call gee_r(a**(-huge(0_1)-1_1))

  call gee_r(a**0_2)
  call gee_r(a**1_2)
  call gee_r(a**2_2)
  call gee_r(a**3_2)
  call gee_r(a**(-1_2))
  call gee_r(a**(-2_2))
  call gee_r(a**(-3_2))
  call gee_r(a**huge(0_2))
  call gee_r(a**(-huge(0_2)))
  call gee_r(a**(-huge(0_2)-1_2))

  call gee_r(a**0_4)
  call gee_r(a**1_4)
  call gee_r(a**2_4)
  call gee_r(a**3_4)
  call gee_r(a**(-1_4))
  call gee_r(a**(-2_4))
  call gee_r(a**(-3_4))
  call gee_r(a**huge(0_4))
  call gee_r(a**(-huge(0_4)))
  call gee_r(a**(-huge(0_4)-1_4))

  call gee_r(a**0_8)
  call gee_r(a**1_8)
  call gee_r(a**2_8)
  call gee_r(a**3_8)
  call gee_r(a**(-1_8))
  call gee_r(a**(-2_8))
  call gee_r(a**(-3_8))
  call gee_r(a**huge(0_8))
  call gee_r(a**(-huge(0_8)))
  call gee_r(a**(-huge(0_8)-1_8))

  ! Complex
  call gee_z(z**0_1)
  call gee_z(z**1_1)
  call gee_z(z**2_1)
  call gee_z(z**3_1)
  call gee_z(z**(-1_1))
  call gee_z(z**(-2_1))
  call gee_z(z**(-3_1))
  call gee_z(z**huge(0_1))
  call gee_z(z**(-huge(0_1)))
  call gee_z(z**(-huge(0_1)-1_1))

  call gee_z(z**0_2)
  call gee_z(z**1_2)
  call gee_z(z**2_2)
  call gee_z(z**3_2)
  call gee_z(z**(-1_2))
  call gee_z(z**(-2_2))
  call gee_z(z**(-3_2))
  call gee_z(z**huge(0_2))
  call gee_z(z**(-huge(0_2)))
  call gee_z(z**(-huge(0_2)-1_2))

  call gee_z(z**0_4)
  call gee_z(z**1_4)
  call gee_z(z**2_4)
  call gee_z(z**3_4)
  call gee_z(z**(-1_4))
  call gee_z(z**(-2_4))
  call gee_z(z**(-3_4))
  call gee_z(z**huge(0_4))
  call gee_z(z**(-huge(0_4)))
  call gee_z(z**(-huge(0_4)-1_4))

  call gee_z(z**0_8)
  call gee_z(z**1_8)
  call gee_z(z**2_8)
  call gee_z(z**3_8)
  call gee_z(z**(-1_8))
  call gee_z(z**(-2_8))
  call gee_z(z**(-3_8))
  call gee_z(z**huge(0_8))
  call gee_z(z**(-huge(0_8)))
  call gee_z(z**(-huge(0_8)-1_8))
end subroutine foo

subroutine gee_i(i)
  integer :: i
end subroutine gee_i

subroutine gee_i8(i)
  integer(kind=8) :: i
end subroutine gee_i8

subroutine gee_r(r)
  real :: r
end subroutine gee_r

subroutine gee_z(c)
  complex :: c
end subroutine gee_z
