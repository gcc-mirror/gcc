! PR fortran/78866
! { dg-do compile }

subroutine pr78866(x)
  integer :: x(*)
!$omp target		! { dg-error "implicit mapping of assumed size array" }
  x(1) = 1
!$omp end target
end
