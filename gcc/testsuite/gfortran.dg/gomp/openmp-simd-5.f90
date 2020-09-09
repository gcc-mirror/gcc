! { dg-additional-options "-fdump-tree-original" }
!
! Related:
!   PR fortran/95109
!   PR fortran/94690
!
implicit none
integer :: i, j, k, ll
integer :: a
!$omp target parallel do simd collapse(1)
  do i = 1, 5
    do j = 1, 5
      do k = 1, 5
        a = a + 1
      end do
      do ll = 1, 5
        a = a + 1
      end do
    end do
  end do
!$omp end target parallel do simd
end

! { dg-final { scan-tree-dump-times "omp simd linear\\(i:1\\) private\\(j\\) private\\(ll\\) private\\(k\\) collapse\\(1\\)" 1 "original" } }
