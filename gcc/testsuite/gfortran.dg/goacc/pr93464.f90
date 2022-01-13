! PR fortran/93464
!
! Contributed by G. Steinmetz

! { dg-additional-options -Wuninitialized }

program p
   character :: c(2) = 'a'
   character, allocatable :: z(:)
   ! { dg-note {'z' declared here} {} { target *-*-* } .-1 }
   !$acc parallel
   ! { dg-warning {'z\.dim\[0\]\.ubound' is used uninitialized} {} { target *-*-* } .-1 }
   ! { dg-warning {'z\.dim\[0\]\.lbound' is used uninitialized} {} { target *-*-* } .-2 }
   !$omp target
   z = c
   !$acc end parallel
   !$omp end target
   print *, z
end
