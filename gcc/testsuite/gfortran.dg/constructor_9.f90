! { dg-do compile }
! { dg-options "-Wall" }
!
! PR 58471: [4.8/4.9 Regression] ICE on invalid with missing type constructor and -Wall
!
! Contributed by Andrew Benson <abensonca@gmail.com>

module cf
  implicit none
  type :: cfmde
  end type
  interface cfmde
     module procedure mdedc   ! { dg-error "is neither function nor subroutine" }
  end interface
contains
  subroutine cfi()
    type(cfmde), pointer :: cfd
    cfd=cfmde()                  ! { dg-error "Can't convert" }
  end subroutine
end module

! { dg-final { cleanup-modules "cf" } }
