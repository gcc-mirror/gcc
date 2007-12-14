! { dg-do compile }
! { dg-options "-std=f95" }
!
! PR fortran/34438
!
! Check that error is not issued for local, non-module
! variables.
!
! Contributed by Sven Buijssen
!
module demo
  implicit none
  private
  type myint
    integer :: bar = 42
  end type myint
  public :: func
contains
  subroutine func()
    type(myint) :: foo
  end subroutine func
end module demo

module demo2
  implicit none
  private
  type myint
    integer :: bar = 42
  end type myint
  type(myint), save :: foo2 ! { dg-error "of PRIVATE derived type" }
  public :: foo2
end module demo2

! { dg-final { cleanup-modules "demo" } }
