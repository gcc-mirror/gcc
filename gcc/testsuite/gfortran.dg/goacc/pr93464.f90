! { dg-additional-options -Wuninitialized }
!
! PR fortran/93464
!
! Contributed by G. Steinmetz
!
! Did before ICE in gfc_omp_check_optional_argument

! Additionally, check for uninitialized warnings.
! Compiled with -O (as done here), no show up;
! for -O0, see testcase file 'pr93464-1.f90'.

program p
   character :: c(2) = 'a'
   character, allocatable :: z(:)
   !$acc parallel
   !$omp target
   ! Remark: As run-time check, required either 'c' being allocated or if(allocated(c)':
   z = c
   !$acc end parallel
   !$omp end target
   print *, z
end
