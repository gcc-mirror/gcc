! PR fortran/70855
! { dg-do compile }
! { dg-additional-options "-O2" }

program pr70855
   integer, parameter :: m = 4
   integer, parameter :: n = 2
   real :: a(m,n)
   real :: x(n)
   real :: y(m)
   a = 1.0
   x = 1.0
!$omp parallel
!$omp workshare
   y(1:m) = matmul ( a(1:m,1:n), x(1:n) )
!$omp end workshare
!$omp end parallel
end program pr70855
