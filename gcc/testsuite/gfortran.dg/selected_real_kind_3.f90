! { dg-do compile }
! { dg-options "-std=f2003" }
!
print *, selected_real_kind(p=precision(0.0),radix=2) ! { dg-error "Fortran 2008" }
print *, selected_real_kind() ! { dg-error "neither 'P' nor 'R' argument" }
end
