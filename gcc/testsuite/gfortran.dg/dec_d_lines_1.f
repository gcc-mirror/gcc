! { dg-do compile }
! { dg-options "-ffixed-form -fd-lines-as-code -fdec" }
!
! Ensure -fd-lines-as-code is not overridden by -fdec.
!
      i = 0
d     end
      subroutine s
D     end
