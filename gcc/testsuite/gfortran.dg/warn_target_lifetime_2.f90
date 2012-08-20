! { dg-do compile }
! { dg-options "-Wtarget-lifetime" }
!
! PR fortran/54301
!
function f()
  integer, pointer :: f
  integer, target :: t
  f => t ! { dg-warning "Pointer at .1. in pointer assignment might outlive the pointer target" }
end
