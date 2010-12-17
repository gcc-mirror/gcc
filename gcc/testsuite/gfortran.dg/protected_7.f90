! { dg-do compile }
! PR fortran/37504
!
module m
  implicit none
  integer, pointer, protected :: protected_pointer
  integer, target,  protected :: protected_target
end module m

program p
  use m
  implicit none
  integer, pointer :: unprotected_pointer
  ! The next two lines should be rejected; see PR 37513 why
  ! we get such a strange error message.
  protected_pointer => unprotected_pointer ! { dg-error "pointer association context" }
  protected_pointer =  unprotected_pointer ! OK
  unprotected_pointer => protected_target  ! { dg-error "target has PROTECTED attribute" }
  unprotected_pointer => protected_pointer ! OK
end program p

! { dg-final { cleanup-modules "m" } }
