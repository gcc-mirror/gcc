! { dg-do compile }
! { dg-additional-options "-fopenmp" }

program p
  integer, allocatable :: r

  allocate (r)
  r = 0

  !$omp target parallel reduction(task, +:r)
  r = r + 1
  !$omp end target parallel
end
