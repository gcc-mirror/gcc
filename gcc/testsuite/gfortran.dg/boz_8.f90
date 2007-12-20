! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/34342
!
! Diagnose BOZ literal for non-integer variables in
! a DATA statement. Cf. Fortran 2003, 5.2.5 DATA statement:
! "If a data-stmt-constant is a boz-literal-constant, the
!  corresponding variable shall be of type integer."
!
real :: r
integer :: i
data i/z'111'/, r/z'4455'/ ! { dg-error "BOZ literal at .1. used to initialize non-integer variable 'r'" }
r = z'FFFF' ! { dg-error "outside a DATA statement" }
i = z'4455' ! { dg-error "outside a DATA statement" }
r = real(z'FFFFFFFFF') ! { dg-error "is too large" }
end
