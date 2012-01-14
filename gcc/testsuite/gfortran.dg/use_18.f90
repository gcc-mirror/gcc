! { dg-do compile }
!
! PR fortran/51816
!
! Contributed by Harald Anlauf 
!
module foo
  implicit none
  type t
     integer :: i
  end type t
  interface operator (*)
     module procedure mult
  end interface
contains
  function mult (i, j)
    type(t), intent(in) :: i, j
    integer             :: mult
    mult = i%i * j%i
  end function mult
end module foo

module bar
  implicit none
  type t2
     integer :: i
  end type t2
  interface operator (>)
     module procedure gt
  end interface
contains
  function gt (i, j)
    type(t2), intent(in) :: i, j
    logical             :: gt
    gt = i%i > j%i
  end function gt
end module bar

use bar, only : t2, operator(>) , operator(>)
use foo, only : t
use foo, only : operator (*)
use foo, only : t
use foo, only : operator (*)
implicit none
type(t) :: i = t(1), j = t(2)
type(t2) :: k = t2(1), l = t2(2)
print *, i*j
print *, k > l
end

! { dg-final { cleanup-modules "foo bar" } }
