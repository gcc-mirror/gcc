! { dg-do run }
! { dg-additional-options "-O2" }
!
! PR fortran/128664 - Scalar structure constructor and BIND(C) interface
!
! Reported by Martin Köhler < martin.pleissa at web.de >

module m
  use, intrinsic :: iso_c_binding, only: c_int16_t
  implicit none

  type, bind(c) :: t
     integer(c_int16_t) :: value
  end type t

  type, bind(c) :: u
     character :: value(4)
  end type u

  interface f
     module function g(vec) bind(C) result(out)
       type(t), intent(in) :: vec(..)
       type(t) :: out
     end function g

     module function h(vec) bind(C) result(out)
       type(u), intent(in) :: vec(..)
       type(u) :: out
     end function h
  end interface f
end module

submodule(m) s
contains
  module function g(vec) bind(C) result(out)
    type(t), intent(in) :: vec(..)
    type(t) :: out
    out%value = 0
  end function g
  module function h(vec) bind(C) result(out)
    type(u), intent(in) :: vec(..)
    type(u) :: out
    out%value = ''
  end function h
end submodule

program demo
  use m
  implicit none
  type(t) :: r
  type(t) :: t1 = t(1)
  type(u) :: s
  type(u) :: u1 = u('a')

  r = f(t1)
  if (r%value /= 0) stop 1
  r = f(t(2.5))
  if (r%value /= 0) stop 2
  s = f(u1)
  if (any (s%value /= '')) stop 3
  s = f(u('b'))
  if (any (s%value /= '')) stop 4
end program demo
