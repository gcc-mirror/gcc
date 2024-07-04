! { dg-add-options vect_early_break }
! { dg-require-effective-target vect_early_break }
! { dg-require-effective-target vect_long_long }
! { dg-additional-options "-fopenmp-simd" }

! { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } }

program main
  integer :: n, i,k
  n = 11
  do i = 1, n,2
    !$omp simd
    do k = 1, i + 41
      if (k > 11 + 41 .or. k < 1) error stop
    end do
  end do
  if (k /= 53) then
    print *, k, 53
    error stop
  endif
end
