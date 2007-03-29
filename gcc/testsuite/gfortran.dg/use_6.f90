! { dg-do compile }
! { dg-options "-std=f95" }
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

use x, only : operator(.bar.) => operator(.addfive.) ! { dg-error "Fortran 2003: Renaming operators in USE statements" }
use y, operator(.my.) => operator(.addfive.) ! { dg-error "Fortran 2003: Renaming operators in USE statements" }
use z
end

! { dg-final { cleanup-modules "x y z" } }
