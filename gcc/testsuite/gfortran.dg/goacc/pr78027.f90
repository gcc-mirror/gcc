! { dg-additional-options "-fopenmp -O2 -fdump-ipa-icf" }

real function f()
   !$omp declare target(f)
   f = 1.
   !$acc parallel
   !$acc loop
   do i = 1, 8
   end do
   !$acc end parallel
   !$acc parallel
   !$acc loop
   do i = 1, 8
   end do
   !$acc end parallel
 end
 
! { dg-final { scan-ipa-dump-times "with total: 0 items" 5 "icf" } }
