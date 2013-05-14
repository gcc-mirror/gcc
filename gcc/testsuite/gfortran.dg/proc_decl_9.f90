! { dg-do run }
! PR33162 INTRINSIC functions as ACTUAL argument
! Test case adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
real function t(x)
  real, intent(in) ::x
  t = x
end function

program p
  implicit none
  intrinsic sin
  procedure(sin):: t
  if (t(1.0) /= 1.0) call abort
end program
