! { dg-do compile }
! PR fortran/119669 - this used to generate an ICE.

program a
  implicit real(a-h,o-z)
  external abstract_caller, caller, func
!  real func
  call abstract_caller (caller, func, 1.5)
  call abstract_caller (caller, func, 1.5)
end program a

function func (x)
  real func, x
  func = x * x - 1.
end
