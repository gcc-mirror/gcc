! { dg-do run }
!
! Tests corrections to implementation of pointer function assignments.
!
! Contributed by Mikael Morin  <mikael.morin@sfr.fr>
!
module m
  implicit none
  type dt
    integer :: data
  contains
    procedure assign_dt
    generic :: assignment(=) => assign_dt
  end type
contains
  subroutine assign_dt(too, from)
    class(dt), intent(out) :: too
    type(dt), intent(in) :: from
    too%data = from%data + 1
  end subroutine
end module m

program p
  use m
  integer, parameter :: b = 3
  integer, target    :: a = 2
  type(dt), target :: tdt
  type(dt) :: sdt = dt(1)

  func (arg=b) = 1         ! This was rejected as an unclassifiable statement
  if (a /= 1) call abort

  func (b + b - 3) = -1
  if (a /= -1) call abort

  dtfunc () = sdt          ! Check that defined assignment is resolved
  if (tdt%data /= 2) call abort
contains
  function func(arg) result(r)
    integer, pointer :: r
    integer :: arg
    if (arg == 3) then
      r => a
    else
      r => null()
    end if
  end function func
  function dtfunc() result (r)
    type(dt), pointer :: r
    r => tdt
  end function
end program p
