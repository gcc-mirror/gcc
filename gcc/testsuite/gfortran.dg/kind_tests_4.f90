! { dg-do compile }
!
! PR 50752: [4.7 Regression] ICE in match_kind_param
!
! Contributed by Joost VandeVondele <Joost.VandeVondele@pci.uzh.ch>

rPos=0.0_dp  ! { dg-error "Missing kind-parameter" }
end
