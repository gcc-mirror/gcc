! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/33106
!
module m1
  implicit none
  type, private :: t
    integer :: i
  end type t
  type(t), public :: one ! { dg-error "PRIVATE derived type" }
  type(t), public, parameter :: two = t(2) ! { dg-error "PRIVATE derived type" }
end module m1

module m2
  implicit none
  private
  type t
    integer :: i
  end type t
  type(t), public :: one ! { dg-error "PRIVATE derived type" }
  type(t), public, parameter :: two = t(2) ! { dg-error "PRIVATE derived type" }
end module m2

module m3
  implicit none
  type t
    integer :: i
  end type t
end module m3

module m4
  use m3!, only: t
  implicit none
  private 
  private :: t
  type(t), public :: one
  type(t), public, parameter :: two = t(2)
end module m4

end
