! { dg-additional-options "-fdump-tree-original -fdump-tree-gimple" }

function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))

  !$omp parallel do collapse(2)
  !$omp tile sizes (8,8)
  do i = 1,m
    do j = 1,n
      inner = 0
      !$omp unroll partial(10)
      do k = 1, n
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end function mult

! { dg-final { scan-tree-dump-times "#pragma omp unroll partial" 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp unroll partial" "gimple" } }
