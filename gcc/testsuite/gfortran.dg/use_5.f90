! { dg-do run }
! Renaming of operators
module z
  interface operator(.addfive.)
    module procedure sub2
  end interface
contains
function sub2(x)
  integer :: sub
  integer,intent(in) :: x
  sub2 = x + 5
end function sub2
end module z

module y
  interface operator(.addfive.)
    module procedure sub
  end interface
contains
function sub(x)
  integer :: sub
  integer,intent(in) :: x
  sub = x + 15
end function sub
end module y

module x
  interface operator(.addfive.)
    module procedure sub
  end interface
contains
function sub(x)
  integer :: sub
  integer,intent(in) :: x
  sub = x + 25
end function sub
end module x

use x, only : operator(.bar.) => operator(.addfive.)
use y, operator(.my.) => operator(.addfive.)
use z
 integer :: i
 i = 2
 if ((.bar. i) /= 2+25) STOP 1
 if ((.my. i) /= 2+15) STOP 2
 if ((.addfive. i) /= 2+5) STOP 3
end
