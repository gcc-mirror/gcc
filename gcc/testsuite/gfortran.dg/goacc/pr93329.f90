! PR fortran/93329
! { dg-do compile { target fopenmp } }
! { dg-additional-options "-fopenmp" }

  integer :: x, y, z
  integer :: a(32)
!$acc kernels copyout(x)
!$omp target map(from:x)	! { dg-error "OMP TARGET directive cannot be specified within" }
  x = 5
!$omp end target
!$acc end kernels
  print *, x
!$acc kernels
!$omp atomic			! { dg-error "OMP ATOMIC directive cannot be specified within" }
  x = x + 1
!$omp end atomic
!$acc end kernels
!$acc kernels
!$omp barrier			! { dg-error "OMP BARRIER directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp cancel parallel		! { dg-error "OMP CANCEL directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp cancellation point parallel	! { dg-error "OMP CANCELLATION POINT directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp flush			! { dg-error "OMP FLUSH directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp distribute		! { dg-error "OMP DISTRIBUTE directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp distribute parallel do	! { dg-error "OMP DISTRIBUTE PARALLEL DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp distribute parallel do simd	! { dg-error "OMP DISTRIBUTE PARALLEL DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp distribute simd		! { dg-error "OMP DISTRIBUTE SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp do simd			! { dg-error "OMP DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp simd			! { dg-error "OMP SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target data map(from: x)	! { dg-error "OMP TARGET DATA directive cannot be specified within" }
!$omp end target data
!$acc end kernels
!$acc kernels
!$omp target enter data map(to: x)	! { dg-error "OMP TARGET ENTER DATA directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp target exit data map(from: x)	! { dg-error "OMP TARGET EXIT DATA directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp target parallel		! { dg-error "OMP TARGET PARALLEL directive cannot be specified within" }
!$omp end target parallel
!$acc end kernels
!$acc kernels
!$omp target parallel do	! { dg-error "OMP TARGET PARALLEL DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target parallel do simd	! { dg-error "OMP TARGET PARALLEL DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target simd		! { dg-error "OMP TARGET SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target teams		! { dg-error "OMP TARGET TEAMS directive cannot be specified within" }
!$omp end target teams
!$acc end kernels
!$acc kernels
!$omp target teams distribute	! { dg-error "OMP TARGET TEAMS DISTRIBUTE directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target teams distribute parallel do	! { dg-error "OMP TARGET TEAMS DISTRIBUTE PARALLEL DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target teams distribute parallel do simd	! { dg-error "OMP TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target teams distribute simd	! { dg-error "OMP TARGET TEAMS DISTRIBUTE SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp target update to(x)	! { dg-error "OMP TARGET UPDATE directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp taskgroup			! { dg-error "OMP TASKGROUP directive cannot be specified within" }
!$omp end taskgroup
!$acc end kernels
!$acc kernels
!$omp taskloop			! { dg-error "OMP TASKLOOP directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp taskloop simd		! { dg-error "OMP TASKLOOP SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp taskwait			! { dg-error "OMP TASKWAIT directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp taskyield			! { dg-error "OMP TASKYIELD directive cannot be specified within" }
!$acc end kernels
!$acc kernels
!$omp teams			! { dg-error "OMP TEAMS directive cannot be specified within" }
!$omp end teams
!$acc end kernels
!$acc kernels
!$omp teams distribute		! { dg-error "OMP TEAMS DISTRIBUTE directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp teams distribute parallel do	! { dg-error "OMP TEAMS DISTRIBUTE PARALLEL DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp teams distribute parallel do simd	! { dg-error "OMP TEAMS DISTRIBUTE PARALLEL DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp teams distribute simd	! { dg-error "OMP TEAMS DISTRIBUTE SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp parallel do		! { dg-error "OMP PARALLEL DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp parallel do simd		! { dg-error "OMP PARALLEL DO SIMD directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
!$acc kernels
!$omp parallel			! { dg-error "OMP PARALLEL directive cannot be specified within" }
!$omp end parallel
!$acc end kernels
!$acc kernels
!$omp parallel sections		! { dg-error "OMP PARALLEL SECTIONS directive cannot be specified within" }
  y = 1
!$omp section
  z = 2
!$omp end parallel sections
!$acc end kernels
!$acc kernels
!$omp sections			! { dg-error "OMP SECTIONS directive cannot be specified within" }
  y = 1
!$omp section
  z = 2
!$omp end sections
!$acc end kernels
!$acc kernels
!$omp ordered			! { dg-error "OMP ORDERED directive cannot be specified within" }
!$omp end ordered
!$acc end kernels
!$acc kernels
!$omp critical			! { dg-error "OMP CRITICAL directive cannot be specified within" }
!$omp end critical
!$acc end kernels
!$acc kernels
!$omp master			! { dg-error "OMP MASTER directive cannot be specified within" }
!$omp end master
!$acc end kernels
!$acc kernels
!$omp single			! { dg-error "OMP SINGLE directive cannot be specified within" }
!$omp end single
!$acc end kernels
!$acc kernels
!$omp task			! { dg-error "OMP TASK directive cannot be specified within" }
!$omp end task
!$acc end kernels
!$acc kernels
!$omp workshare			! { dg-error "OMP WORKSHARE directive cannot be specified within" }
  a(:) = 1
!$omp end workshare
!$acc end kernels
!$acc kernels
!$omp parallel workshare	! { dg-error "OMP PARALLEL WORKSHARE directive cannot be specified within" }
  a(:) = 1
!$omp end parallel workshare
!$acc end kernels
!$acc kernels
!$omp do			! { dg-error "OMP DO directive cannot be specified within" }
  do x = 0, 2
  end do
!$acc end kernels
end
