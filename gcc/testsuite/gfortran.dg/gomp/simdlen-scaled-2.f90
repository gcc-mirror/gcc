! { dg-do compile }
! { dg-additional-options "-fdump-tree-original -w" }

! PR middle-end/127723

subroutine f(n, a, d)
  integer :: i, n, a(*)
  real :: d(*)

  !$omp simd simdlen(scaled(integer,1): 1)  ! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  do i = 1, 1
    a(i) = a(i) + 5
  end do

  !$omp simd simdlen(scaled(integer(8)): 2)  ! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  do i = 1, 1
    a(i) = a(i) + 5
  end do

  !$omp simd simdlen(scaled(complex(4), 4): 8)  ! { dg-message "sorry, unimplemented: 'simdlen' clause with 'scaled' modifier" }
  do i = 1, 1
    d(i) = 0.0
  end do
end

! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) simdlen\\(scaled\\(integer\\(kind=4\\),1\\):1\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) simdlen\\(scaled\\(integer\\(kind=8\\)\\):2\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp simd linear\\(i:1\\) simdlen\\(scaled\\(complex\\(kind=4\\),4\\):8\\)" "original" } }
