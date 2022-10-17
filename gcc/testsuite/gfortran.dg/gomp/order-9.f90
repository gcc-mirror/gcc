subroutine foo
  !$omp do schedule(static) order(concurrent) order(concurrent) ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp do schedule(static) order(reproducible:concurrent) order(unconstrained:concurrent)      ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do

  !$omp loop bind(thread) order(concurrent) order(concurrent)    ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp loop bind(thread) order(reproducible:concurrent) order(unconstrained:concurrent) ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp simd order(concurrent) order(concurrent) ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp simd order(reproducible:concurrent) order(unconstrained:concurrent)      ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp distribute dist_schedule(static) order(concurrent) order(concurrent)     ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
  !$omp loop bind(thread) order(reproducible:concurrent) order(unconstrained:concurrent) ! { dg-error "Duplicated 'order \\(' clause" }
  do i = 1, 8
    call f0 ()
  end do
end
