! { dg-additional-options "-Wuninitialized -O0" }
!
! PR fortran/93464
!
! Contributed by G. Steinmetz
!
! Did before ICE in gfc_omp_check_optional_argument

! Additionally, check for uninitialized warnings. There are
! none with -O (cf. original testcase file 'pr93464.f90').
! For -O0, see below:

program p
   character :: c(2) = 'a'
   character, allocatable :: z(:)
   ! { dg-note {'z' declared here} {} { target *-*-* } .-1 }
   !$acc parallel
   ! { dg-warning {'z\.dim\[0\]\.ubound' may be used uninitialized} {} { target *-*-* } .-1 }
   ! { dg-warning {'z\.dim\[0\]\.lbound' may be used uninitialized} {} { target *-*-* } .-2 }
   !$omp target
   ! Remark: As run-time check, required either 'c' being allocated or if(allocated(c)':
   z = c
   !$acc end parallel
   !$omp end target
   print *, z
end
