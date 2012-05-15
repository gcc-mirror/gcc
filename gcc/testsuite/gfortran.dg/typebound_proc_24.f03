! { dg-do compile }
!
! PR 49112: [4.6/4.7 Regression] [OOP] Missing type-bound procedure, "duplicate save" warnings and internal compiler error
!
! Contributed by John <jwmwalrus@gmail.com>

module datetime_mod

  implicit none

  type :: DateTime
    integer :: year, month, day
  contains
    procedure :: getFormattedString
  end type

  type(DateTime) :: ISO_REFERENCE_DATE = DateTime(1875, 5, 20)

contains

  character function getFormattedString(dt)
    class(DateTime) :: dt
  end function

  subroutine test
    type(DateTime) :: dt
    print *,dt%getFormattedString()
  end subroutine

end module 
