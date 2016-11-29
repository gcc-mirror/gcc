! PR fortran/78026
select type (a)		! { dg-error "Selector shall be polymorphic in SELECT TYPE statement" }
end select
!$omp declare simd(b)	! { dg-error "Unexpected !.OMP DECLARE SIMD statement" }
end			! { dg-error "should refer to containing procedure" "" { target *-*-* } .-1 }
