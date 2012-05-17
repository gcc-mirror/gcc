! { dg-do compile }
! Tests the for PR30410, in which the reference to extfunc would
! be incorrectly made to the module namespace.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
module mod1
contains
  function eval (func, x1)
    real     :: eval, func, x1
    external :: func
    eval = func (x1)
  end function eval
end module mod1
!-------------------------------
module mod2
  use mod1, only : eval
  real, external :: extfunc     ! This was referenced as __mod2__extfunc__
contains

  subroutine foo (x0)
    real :: x0, x1
    x1 = 42
    x0 = eval (extfunc, x1)
  end subroutine foo

end module mod2
!-------------------------------
function extfunc (x)
  real, intent(in) ::  x
  real             ::  extfunc
  extfunc = x
end function extfunc
!-------------------------------
program gfcbug53
  use mod2, only : foo
  real :: x0 = 0
  call foo (x0)
  print *, x0
end program gfcbug53
