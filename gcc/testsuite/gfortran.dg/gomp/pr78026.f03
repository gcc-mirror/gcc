! PR fortran/78026
select type (a)		! { dg-error "Selector shall be polymorphic in SELECT TYPE statement" }
end select
!$omp declare simd(b)	! { dg-error "\\!\\$OMP DECLARE SIMD statement at \\(1\\) cannot appear after executable statements" }
end			! { dg-error "should refer to containing procedure" "" { target *-*-* } .-1 }
