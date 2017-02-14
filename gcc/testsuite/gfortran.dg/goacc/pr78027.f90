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
 
! { dg-final { scan-ipa-dump "Not parsed function:f_._omp_fn.1" "icf" } }
! { dg-final { scan-ipa-dump "Not parsed function:f_._omp_fn.0" "icf" } }
! { dg-final { scan-ipa-dump "Not parsed function:f_" "icf" } }
