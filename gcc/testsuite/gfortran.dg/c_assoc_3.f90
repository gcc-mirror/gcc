! { dg-do compile }
!
! PR fortran/43303
!
! Contributed by Dennis Wassel
!
PROGRAM c_assoc
  use iso_c_binding
  type(c_ptr) :: x
  x = c_null_ptr
  print *, C_ASSOCIATED(x) ! <<< was ICEing here
  if (C_ASSOCIATED(x)) call abort ()
END PROGRAM c_assoc
