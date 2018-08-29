! { dg-do compile }
! { dg-additional-options "-Wall -Wextra -Wno-unused-dummy-argument" }
! PR fortran/70330 - this used to cause an ICE.
! Test case by Vladimir Fuka
function f(o) ! { dg-warning "Return value of function" }
  optional o
end function f
