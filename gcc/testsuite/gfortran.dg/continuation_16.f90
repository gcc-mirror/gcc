! PR fortran/89724
! { dg-do compile }
! { dg-options "-std=f95 -nostdinc -fpre-include=simd-builtins-1.h" }
  &  
&
 &
end
! { dg-warning "not allowed by itself in line 4" "" { target *-*-* } 0 }
! { dg-warning "not allowed by itself in line 5" "" { target *-*-* } 0 }
! { dg-warning "not allowed by itself in line 6" "" { target *-*-* } 0 }
