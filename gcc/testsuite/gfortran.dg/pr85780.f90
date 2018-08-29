! { dg-do compile }
! { dg-options "-std=legacy" }
! PR fortran/85780
subroutine s(*) bind(c)
end
