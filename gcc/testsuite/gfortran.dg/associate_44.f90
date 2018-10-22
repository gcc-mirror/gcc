! { dg-do compile }
!
! Test the fix for PR56386
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
subroutine  CustomSolidBodies
   implicit none

    type inner
      real :: elev
    end type

    type :: outer
      type(inner),dimension(0) :: PrPoints
    end type

    type(outer) :: SB

    associate (Prter=>SB%PrPoints)
       PrTer%elev=0                  ! ICE here
    end associate
end subroutine  CustomSolidBodies
