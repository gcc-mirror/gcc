! PR fortran/43836
! { dg-do compile }
! { dg-options "-fopenmp -fexceptions -O2" }
subroutine foo
!$omp single
!$omp parallel
  call bar
!$omp end parallel
!$omp end single
end subroutine foo
