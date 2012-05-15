! { dg-do compile }
! Renaming of operators
module z
  type myT
    integer :: t
  end type myT
  interface operator(+)
    module procedure sub2
  end interface
contains
function sub2(x)
  type(myT) :: sub2
  type(myT),intent(in) :: x
  sub2%t = x%t + 5
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

use z, operator(-) => operator(+) ! { dg-error "Syntax error in USE statement" }
use z, operator(.op.) => operator(+) ! { dg-error "Syntax error in USE statement" }
use x, only : bar => operator(.addfive.) ! { dg-error "Syntax error in USE statement" }
use y, operator(.my.) => sub ! { dg-error "Syntax error in USE statement" }
use y, operator(+) => operator(.addfive.) ! { dg-error "Syntax error in USE statement" }
end
