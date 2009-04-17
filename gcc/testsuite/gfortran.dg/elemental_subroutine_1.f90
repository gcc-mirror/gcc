! { dg-do run }
! Test the fix for pr22146, where and elemental subroutine with
! array actual arguments would cause an ICE in gfc_conv_function_call.
! The module is the original test case and the rest is a basic
! functional test of the scalarization of the function call.
!
! Contributed by Erik Edelmann  <erik.edelmann@iki.fi>
!             and Paul Thomas   <pault@gcc.gnu.org>

  module pr22146

contains

    elemental subroutine foo(a)
      integer, intent(out) :: a
      a = 0
    end subroutine foo

    subroutine bar()
      integer :: a(10)
      call foo(a)
    end subroutine bar

end module pr22146

  use pr22146
  real, dimension (2)  :: x, y
  real :: u, v
  x = (/1.0, 2.0/)
  u = 42.0

  call bar ()

! Check the various combinations of scalar and array.
  call foobar (x, y)
  if (any(y.ne.-x)) call abort ()

  call foobar (u, y)
  if (any(y.ne.-42.0)) call abort ()

  call foobar (u, v)
  if (v.ne.-42.0) call abort ()

  v = 2.0
  call foobar (v, x)
  if (any(x /= -2.0)) call abort ()

! Test an expression in the INTENT(IN) argument
  x = (/1.0, 2.0/)
  call foobar (cos (x) + u, y)
  if (any(abs (y + cos (x) + u) .gt. 4.0e-6)) call abort ()

contains

  elemental subroutine foobar (a, b)
    real, intent(IN) :: a
    real, intent(out) :: b
    b = -a
  end subroutine foobar
end

! { dg-final { cleanup-modules "pr22146" } }
