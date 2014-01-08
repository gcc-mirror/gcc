! { dg-do compile }
!
! PR 58182: [4.9 Regression] ICE with global binding name used as a FUNCTION
!
! Contributed by Andrew Bensons <abensonca@gmail.com>
!
! This file must be compiled AFTER binding_label_tests_26a.f90, which it 
! should be because dejagnu will sort the files.  

module f    ! { dg-error "uses the same global identifier" }
  use fg    ! { dg-error "uses the same global identifier" }
end module

! { dg-final { cleanup-modules "fg f" } }
