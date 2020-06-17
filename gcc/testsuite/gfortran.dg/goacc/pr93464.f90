! { dg-do compile }
!
! PR fortran/93464
!
! Contributed by G. Steinmetz
!
program p
   character :: c(2) = 'a'
   character, allocatable :: z(:)
   !$acc parallel
   !$omp target
   z = c
   !$acc end parallel
   !$omp end target
   print *, z
end
