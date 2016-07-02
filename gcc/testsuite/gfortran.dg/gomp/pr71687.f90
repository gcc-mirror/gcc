! PR fortran/71687
! { dg-do compile }
! { dg-additional-options "-fstack-arrays -O2" }

subroutine s (n, x)
   integer :: n
   real :: x(n)
!$omp parallel
   x(1:n) = x(n:1:-1)
!$omp end parallel
end
