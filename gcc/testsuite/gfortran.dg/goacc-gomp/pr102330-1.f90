! { dg-additional-options -fopt-info-omp-note }

! { dg-additional-options --param=openacc-privatization=noisy }

subroutine r1
  !$omp master taskloop simd
  do i = 1, 8
  end do
  !$acc parallel loop ! { dg-line l_compute1 }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute1 }
  do i = 1, 8
  end do
end

subroutine r2
  !$omp taskloop lastprivate(i)
  do i = 1, 8
  end do
  !$acc parallel loop ! { dg-line l_compute2 }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute2 }
  do i = 1, 8
  end do
end

subroutine r3
  i = 0
  !$omp task shared(i)
  i = 1
  !$omp end task
  !$omp taskwait
  !$acc parallel loop ! { dg-line l_compute3 }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute3 }
  do i = 1, 8
  end do
end
