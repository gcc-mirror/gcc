! { dg-do compile }
!
! PR 58355: [4.7/4.8/4.9 Regression] [F03] ICE with TYPE, EXTENDS before parent TYPE defined
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module ct
  public :: t1

  type, extends(t1) :: t2   ! { dg-error "has not been previously defined" }

  type :: t1
  end type
end
