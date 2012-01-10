! { dg-do compile }
!
! PR fortran/51308
!
! Contributed by Matthias Moeller
!

module mymod
  use iso_c_binding
  implicit none

  private
  public :: c_ptr
  public :: c_null_ptr

end module mymod

! { dg-final { cleanup-modules "mymod" } }
