! { dg-additional-options -fchecking }
! { dg-ice TODO }

! { dg-additional-options -fopt-info-omp-note }

! { dg-additional-options --param=openacc-privatization=noisy }

program p
  !$omp master taskloop simd
  do i = 1, 8
  end do
  !$acc parallel loop ! { dg-line l_compute1 }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_compute1 }
  do i = 1, 8
  end do
end
! { dg-bogus {Error: non-register as LHS of binary operation} TODO { target { ! offloading_enabled } xfail *-*-* } .-1 }
! { dg-bogus {error: non-register as LHS of binary operation} TODO { target offloading_enabled xfail *-*-* } .-2 }
! TODO See PR101551 for 'offloading_enabled' differences.
! { dg-excess-errors ICE }
