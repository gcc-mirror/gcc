! { dg-do compile }
! PR fortran/43592
! Original code submitted by Joost VandeVondele
! Dejagnu-ification by Steven G. Kargl
! 
 interface assignment (=)
  interface pseudo_scalar  ! { dg-error "Unexpected INTERFACE statement" }
  pure function double_tensor2odd (x, t2) result (xt2)
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
