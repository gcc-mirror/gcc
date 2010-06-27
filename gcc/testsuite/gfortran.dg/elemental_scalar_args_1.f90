! { dg-do compile }
! Test the fix for PR43843, in which the temporary for b(1) in
! test_member was an indirect reference, rather then the value.
!
! Contributed by Kyle Horne <horne.kyle@gmail.com>
! Reported by Tobias Burnus <burnus@gcc.gno.org>
! Reported by Harald Anlauf <anlauf@gmx.de> (PR43841)
!
module polar_mod
  implicit none
  complex, parameter :: i = (0.0,1.0)
  real, parameter :: pi = 3.14159265359
  real, parameter :: e = exp (1.0)
  type :: polar_t
    real :: l, th
  end type
  type(polar_t) :: one = polar_t (1.0, 0)
  interface operator(/)
    module procedure div_pp
  end interface
  interface operator(.ne.)
    module procedure ne_pp
  end interface
contains
  elemental function div_pp(u,v) result(o)
    type(polar_t), intent(in) :: u, v
    type(polar_t) :: o
    complex :: a, b, c
    a = u%l*exp (i*u%th*pi)
    b = v%l*exp (i*v%th*pi)
    c = a/b
    o%l = abs (c)
    o%th = atan2 (imag (c), real (c))/pi
  end function div_pp
  elemental function ne_pp(u,v) result(o)
    type(polar_t), intent(in) :: u, v
    LOGICAL :: o
    if (u%l .ne. v%l) then
      o = .true.
    else if (u%th .ne. v%th) then
      o = .true.
    else
      o = .false.
    end if
  end function ne_pp
end module polar_mod

program main
  use polar_mod
  implicit none
  call test_member
  call test_other
  call test_scalar
  call test_real
contains
  subroutine test_member
    type(polar_t), dimension(3) :: b
    b = polar_t (2.0,0.5)
    b(:) = b(:)/b(1)
    if (any (b .ne. one)) call abort   
  end subroutine test_member
  subroutine test_other
    type(polar_t), dimension(3) :: b
    type(polar_t), dimension(3) :: c
    b = polar_t (3.0,1.0)
    c = polar_t (3.0,1.0)
    b(:) = b(:)/c(1)
    if (any (b .ne. one)) call abort   
  end subroutine test_other
  subroutine test_scalar
    type(polar_t), dimension(3) :: b
    type(polar_t) :: c
    b = polar_t (4.0,1.5)
    c = b(1)
    b(:) = b(:)/c
    if (any (b .ne. one)) call abort   
  end subroutine test_scalar
  subroutine test_real
    real,dimension(3) :: b
    real :: real_one
    b = 2.0
    real_one = b(2)/b(1)
    b(:) = b(:)/b(1)
    if (any (b .ne. real_one)) call abort   
  end subroutine test_real
end program main
! { dg-final { cleanup-modules "polar_mod" } }
