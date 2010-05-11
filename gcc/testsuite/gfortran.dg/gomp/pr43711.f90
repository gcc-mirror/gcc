! { dg-do "compile" }
! { dg-options "-fopenmp" }
!
! PR fortran/43711 uninformative error message for two 'nowait' in omp statement
! Contributed by Bill Long <longb AT cray DOT com>

program NF03_2_5_2_1a
   !$omp parallel
      !$omp sections
      !$omp section
         print *, 'FAIL'
      !$omp section
         print *, 'FAIL'
      !$omp end sections nowait nowait     ! { dg-error "Unexpected junk" }
   !$omp end parallel
end program NF03_2_5_2_1a

! { dg-excess-errors "Unexpected" }