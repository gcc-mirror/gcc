! { dg-do run }
!
! PR 43256: [OOP] TBP with missing optional arg
!
! Contributed by Janus Weil

module module_myobj

  implicit none

  type :: myobj
  contains
    procedure, nopass :: myfunc
  end type

contains

  integer function myfunc(status)
    integer, optional :: status
    if (present(status)) then
      myfunc = 1
    else
      myfunc = 2
    end if
  end function

end module


program test_optional

  use :: module_myobj
  implicit none

  integer     :: res = 0
  type(myobj) :: myinstance

  res = myinstance%myfunc()
  if (res /= 2) STOP 1

end program
