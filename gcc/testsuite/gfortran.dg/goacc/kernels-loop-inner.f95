! { dg-additional-options "--param openacc-kernels=decompose-parloops" } as this is
! specifically testing "parloops" handling.
! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-optimized-omp" }

program main
   implicit none

   integer :: a(100,100), b(100,100)
   integer :: i, j, d

   !$acc kernels
   do i=1,10 ! { dg-message "optimized: assigned OpenACC seq loop parallelism" }0
     do j=1,100
       a(i,j) = 1
       b(i,j) = 2
       a(i,j) = a(i,j) + b(i,j)
     end do
   end do
   !$acc end kernels

   d = sum(a)

   print *,d
end program main
