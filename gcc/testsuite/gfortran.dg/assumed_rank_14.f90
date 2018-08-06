! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR fortran/83184
!

integer n1(..) /1/
! { dg-error "Assumed-rank array.*must be a dummy argument" "" { target *-*-* } 7 }
! { dg-error "Assumed-rank variable.*actual argument" "" { target *-*-* } 7 }

end
