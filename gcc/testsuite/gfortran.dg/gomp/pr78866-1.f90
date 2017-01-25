! PR fortran/78866
! { dg-do compile }

subroutine pr78866(x)
  integer :: x(*)
!$omp target map(x)			! { dg-error "Assumed size array" }
  x(1) = 1
!$omp end target
!$omp target data map(tofrom: x)	! { dg-error "Assumed size array" }
!$omp target update to(x)		! { dg-error "Assumed size array" }
!$omp target update from(x)		! { dg-error "Assumed size array" }
!$omp end target data
!$omp target map(x(:23))		! { dg-bogus "Assumed size array" }
  x(1) = 1
!$omp end target
!$omp target map(x(:))			! { dg-error "upper bound of assumed size array section" }
  x(1) = 1				! { dg-error "not a proper array section" "" { target *-*-* } .-1 }
!$omp end target
end
