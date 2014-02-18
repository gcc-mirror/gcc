! { dg-do compile }
!
! PR 60231: [4.8/4.9 Regression] ICE on undefined generic
!
! Contributed by Antony Lewis <antony@cosmologist.info>

module Objects

  Type TObjectList
  contains
    procedure :: Add1             ! { dg-error "must be a module procedure" }
    procedure :: Add2             ! { dg-error "must be a module procedure" }
    generic :: Add => Add1, Add2  ! { dg-error "are ambiguous" }
  end Type

end module

! { dg-final { cleanup-modules "Objects" } }
