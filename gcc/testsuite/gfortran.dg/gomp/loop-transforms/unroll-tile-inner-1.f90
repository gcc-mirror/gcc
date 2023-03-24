! { dg-additional-options "-fdump-tree-original" }
! { dg-additional-options "-fdump-tree-omp_transform_loops" }

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

! { dg-final { scan-tree-dump-times "#pragma omp loop_transform unroll_partial" 1 "original" } }
! { dg-final { scan-tree-dump-not "#pragma omp loop_transform unroll_partial" "omp_transform_loops" } }
