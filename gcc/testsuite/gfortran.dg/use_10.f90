! { dg-do run }
module a
 implicit none
interface operator(.op.)
  module procedure sub
end interface
interface operator(.ops.)
  module procedure sub2
end interface

contains
  function sub(i)
    integer :: sub
    integer,intent(in) :: i
    sub = -i
  end function sub
  function sub2(i)
    integer :: sub2
    integer,intent(in) :: i
    sub2 = i
  end function sub2
end module a

program test
use a, only: operator(.op.), operator(.op.), &
operator(.my.)=>operator(.op.),operator(.ops.)=>operator(.op.)
implicit none
if (.my.2 /= -2 .or. .op.3 /= -3 .or. .ops.7 /= -7) STOP 1
end
