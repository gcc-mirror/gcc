! Invalid use of routines defined inside a Fortran module.

! { dg-compile-aux-modules "routine-module-mod-1.f90" }

subroutine sr_1
  use routine_module_mod_1
  implicit none

  !$acc routine (s_1) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_1" }
   ! { dg-error "NAME 's_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_1_nh) seq nohost ! { dg-error "Cannot change attributes of USE-associated symbol s_1_nh" }
   ! { dg-error "NAME 's_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_2) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_2" }
   ! { dg-error "NAME 's_2' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_2_nh) seq nohost ! { dg-error "Cannot change attributes of USE-associated symbol s_2_nh" }
   ! { dg-error "NAME 's_2_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (v_1) seq ! { dg-error "Cannot change attributes of USE-associated symbol v_1" }
   ! { dg-error "NAME 'v_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (v_1_nh) seq nohost ! { dg-error "Cannot change attributes of USE-associated symbol v_1_nh" }
   ! { dg-error "NAME 'v_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (w_1) gang ! { dg-error "Cannot change attributes of USE-associated symbol w_1" }
   ! { dg-error "NAME 'w_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (w_1_nh) gang nohost ! { dg-error "Cannot change attributes of USE-associated symbol w_1_nh" }
   ! { dg-error "NAME 'w_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (g_1) gang ! { dg-error "Cannot change attributes of USE-associated symbol g_1" }
   ! { dg-error "NAME 'g_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (g_1_nh) gang nohost ! { dg-error "Cannot change attributes of USE-associated symbol g_1_nh" }
   ! { dg-error "NAME 'g_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
end subroutine sr_1

subroutine sr_2
  use routine_module_mod_1
  implicit none

  !$acc routine (s_1) seq nohost ! { dg-error "Cannot change attributes of USE-associated symbol s_1" }
   ! { dg-error "NAME 's_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_1_nh) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_1_nh" }
   ! { dg-error "NAME 's_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_2) seq nohost ! { dg-error "Cannot change attributes of USE-associated symbol s_2" }
   ! { dg-error "NAME 's_2' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_2_nh) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_2_nh" }
   ! { dg-error "NAME 's_2_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (v_1) vector nohost ! { dg-error "Cannot change attributes of USE-associated symbol v_1" }
   ! { dg-error "NAME 'v_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (v_1_nh) vector ! { dg-error "Cannot change attributes of USE-associated symbol v_1_nh" }
   ! { dg-error "NAME 'v_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (w_1) worker nohost ! { dg-error "Cannot change attributes of USE-associated symbol w_1" }
   ! { dg-error "NAME 'w_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (w_1_nh) worker ! { dg-error "Cannot change attributes of USE-associated symbol w_1_nh" }
   ! { dg-error "NAME 'w_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (g_1) worker nohost ! { dg-error "Cannot change attributes of USE-associated symbol g_1" }
   ! { dg-error "NAME 'g_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (g_1_nh) worker ! { dg-error "Cannot change attributes of USE-associated symbol g_1_nh" }
   ! { dg-error "NAME 'g_1_nh' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
end subroutine sr_2
