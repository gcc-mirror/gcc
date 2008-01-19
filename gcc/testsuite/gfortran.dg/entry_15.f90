! { dg-do compile }
! 
! PR fortran/34137
!
! Entry was previously not possible in a module.
! Checks also whether the different result combinations
! work properly.
!
module m2
  implicit none
contains
function func(a)
  implicit none
  integer :: a, func
  real :: func2
  func = a*8
  return
entry ent(a) result(func2)
  ent = -a*4.0 ! { dg-error "is not a variable" }
  return
end function func
end module m2

module m3
  implicit none
contains
function func(a) result(res)
  implicit none
  integer :: a, res
  real :: func2
  res = a*12
  return
entry ent(a) result(func2)
  ent = -a*6.0 ! { dg-error "is not a variable" }
  return
end function func
end module m3
