! Invalid use of routines defined inside a Fortran module.

! { dg-compile-aux-modules "routine-module-mod-1.f90" }

program main
  use routine_module_mod_1
  implicit none
  !$acc routine (s_1) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_1" }
   ! { dg-error "NAME 's_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (s_2) seq ! { dg-error "Cannot change attributes of USE-associated symbol s_2" }
   ! { dg-error "NAME 's_2' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (v_1) seq ! { dg-error "Cannot change attributes of USE-associated symbol v_1" }
   ! { dg-error "NAME 'v_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
  !$acc routine (w_1) gang ! { dg-error "Cannot change attributes of USE-associated symbol w_1" }
   ! { dg-error "NAME 'w_1' invalid in \\!\\\$ACC ROUTINE \\( NAME \\)" "" { target *-*-* } .-1 }
end program main
