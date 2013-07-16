! { dg-do compile }
! { dg-options "-Wall -Wno-uninitialized" }
!
! PR fortran/56477
! The pointer target live range checking code used to trigger a NULL pointer
! dereference with the following case.
!
! Contributed by Andrew Benson <abensonca@gmail.com>
!
module s
contains
  function so()
    implicit none
    integer, target  :: so
    integer, pointer :: sp
    sp => so
    return
  end function So
end module s
