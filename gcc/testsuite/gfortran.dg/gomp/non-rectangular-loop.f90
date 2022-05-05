! { dg-do compile }
! { dg-options "-fopenmp" }

! Test that errors are given for cases where there are constraints
! disallowing nonrectangular loops.

! Work-sharing loop disallows "schedule" and "ordered" clauses.

subroutine s1 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp do collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp do collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp do collapse(2) ordered  ! { dg-error "'ordered' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp do collapse(2) ordered  ! { dg-error "'ordered' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  ! Derived constructs

  !$omp do simd collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp parallel do collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp parallel do simd collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp target parallel do collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp target parallel do collapse(2) schedule(static)  ! { dg-error "'schedule' clause may not appear on non-rectangular 'do'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

end subroutine


! Distribute construct disallows "dist_schedule" clause.

subroutine s2 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp distribute collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp distribute collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  ! Derived constructs

  !$omp distribute simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp distribute parallel do collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp distribute parallel do simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp teams distribute collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp teams distribute simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp teams distribute parallel do collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp teams distribute parallel do simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp target teams distribute collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp target teams distribute simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp target teams distribute parallel do collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp target teams distribute parallel do simd collapse(2) dist_schedule(static)  ! { dg-error "'dist_schedule' clause may not appear on non-rectangular 'distribute'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

end subroutine

! Taskloop construct disallows "grainsize" and "num_tasks" clauses.

subroutine s3 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp taskloop collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp taskloop collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp taskloop collapse(2) num_tasks(4)  ! { dg-error "'num_tasks' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp taskloop collapse(2) num_tasks(4)  ! { dg-error "'num_tasks' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = i, 16
    end do
  end do

  ! Derived constructs

  !$omp taskloop simd collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp masked taskloop collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp masked taskloop simd collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp parallel masked taskloop collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp parallel masked taskloop simd collapse(2) grainsize(4)  ! { dg-error "'grainsize' clause may not appear on non-rectangular 'taskloop'" }
  do i = 1, 16
    do j = 1, i
    end do
  end do

end subroutine

! TODO: not yet implemented
! The tile construct disallows all non-rectangular loops.


