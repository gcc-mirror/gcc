! { dg-do compile }
! tests the fix for PR31540, in which the character lengths in
! parentheses were not resolved.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
        subroutine pfb()
        implicit none
        external pfname1, pfname2
        character ((136)) pfname1
        character ((129+7)) pfname2
        return
        end
