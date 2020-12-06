! { dg-do compile { target { ! *-*-* } } }
! SKIP THIS FILE
!
! Used by codimension_2.f90
!
! Additional file to check that using the module doesn't generate
! a token symbol. (The module is also used by codimension_2.f90.)
!
subroutine ttest
  use global_coarrays
  implicit none
  b(:) = b(:)[2]
end
