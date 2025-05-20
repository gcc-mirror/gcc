! Compiled with pr120049_b.f90
! { dg-options -O0 }
! { dg-do run }
! { dg-compile-aux-modules "pr120049_b.f90" }
! { dg-additional-sources pr120049_b.f90 }
!
! Test the fix for PR86248
program tests_gtk_sup
  use gtk_sup
  implicit none
  type(c_ptr), target :: val
  if (c_associated(val, c_loc(val))) then
    stop 1
  endif
  if (c_associated(c_loc(val), val)) then
    stop 2
  endif
end program tests_gtk_sup
