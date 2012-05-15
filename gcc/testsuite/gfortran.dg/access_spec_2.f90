! { dg-do compile }
! { dg-options "-std=f95" }
! PR fortran/31472
! Access specifications: Invalid Fortran 95 code

module test
  implicit none
  integer, public :: x
  public :: x  ! { dg-error "was already specified" }
  private :: x ! { dg-error "was already specified" }
end module test

module mod
  implicit none
  private
  type, public :: bar
    PRIVATE
    integer, public :: y  ! { dg-error "Fortran 2003: Attribute PUBLIC" }
    integer, public :: z  ! { dg-error "Fortran 2003: Attribute PUBLIC" }
  end type ! { dg-error "Derived type definition at" }
contains
  subroutine foo
     integer :: x
     private :: x ! { dg-error "only allowed in the specification part of a module" }
     type, private :: t ! { dg-error "only be PRIVATE in the specification part of a module" }
        integer :: z
     end type t ! { dg-error "Expecting END SUBROUTINE statement" }
     type :: ttt
        integer,public :: z ! { dg-error "not allowed outside of the specification part of a module" }
     end type ttt ! { dg-error "Derived type definition at" }
  end subroutine
end module

program x
  implicit none
  integer :: i
  public  :: i ! { dg-error "only allowed in the specification part of a module" }
  integer,public :: j ! { dg-error "not allowed outside of the specification part of a module" }
end program x
