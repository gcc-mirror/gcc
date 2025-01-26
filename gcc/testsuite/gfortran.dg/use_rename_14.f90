! { dg-do compile }
!
! PR fortran/110993 - bogus diagnostics on renamed interface import
!
! Contributed by Rimvydas Jasinskas <rimvydas.jas at gmail.com>

module m
  interface
    subroutine bar(x)
      use iso_c_binding, only : c_float
      implicit none
      real(c_float) :: x(45)
    end subroutine
  end interface
end

module m1
  interface
    subroutine bar1(x) bind(c)
      use iso_c_binding, only : c_float
      implicit none
      real(c_float) :: x(45)
    end subroutine
  end interface
end

module m2
  interface
    subroutine bar2(x) bind(c, name="bar2_")
      use iso_c_binding, only : c_float
      implicit none
      real(c_float) :: x(45)
    end subroutine
  end interface
end

subroutine foo(y)
  use m,  notthisone => bar
  use m1, northisone => bar1
  use m2,  orthisone => bar2
  implicit none
  real :: y(3)
  call bar (y)
  call bar1(y)
  call bar2(y)
end subroutine
