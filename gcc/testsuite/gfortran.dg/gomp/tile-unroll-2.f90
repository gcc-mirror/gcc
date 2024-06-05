function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))

  !$omp parallel do collapse(2)
  !$omp tile sizes (8,8)
  !$omp unroll partial(2) ! { dg-error "UNROLL construct at \\\(1\\\) with PARTIAL clause generates just one loop with canonical form but 2 loops are needed" }
  do i = 1,m
    do j = 1,n
      inner = 0
      do k = 1, n
        inner = inner + a(k, i) * b(j, k)
      end do
    c(j, i) = inner
    end do
  end do

  !$omp tile sizes (8,8)
  !$omp unroll partial(2) ! { dg-error "UNROLL construct at \\\(1\\\) with PARTIAL clause generates just one loop with canonical form but 2 loops are needed" }
  do i = 1,m
    do j = 1,n
      inner = 0
      do k = 1, n
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do

  !$omp parallel do collapse(2)
  !$omp tile sizes (8,8)
  !$omp unroll full ! { dg-error "Generated loop of UNROLL construct at \\\(1\\\) without PARTIAL clause does not have canonical form" }
  do i = 1,m
    do j = 1,n
      inner = 0
      do k = 1, n
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end function mult
