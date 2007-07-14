! { dg-do compile }
!
! PR fortran/32724
! ICE on statement function in specification part of module

MODULE stmt
f(x) = x**2      ! { dg-error "Unexpected STATEMENT FUNCTION" }
END MODULE

! { dg-final { cleanup-modules "stmt" } }

