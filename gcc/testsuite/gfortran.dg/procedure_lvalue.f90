! { dg-do compile }
! Tests the fix for PR17911, where a USE associated l-value
! would cause an ICE in gfc_conv_variable.
! Test contributed by Tobias Schlueter  <tobi@gcc.gnu.org>
module t
  interface a
     module procedure b
  end interface
contains
  integer function b(x)
    b = x
  end function b
end module t

subroutine r
  use t
  b = 1.       ! { dg-error "is not a variable" }
  y = a(1.)
end subroutine r

! { dg-final { cleanup-modules "t" } }
