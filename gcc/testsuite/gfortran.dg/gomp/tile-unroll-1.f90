function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))

  !$omp tile sizes (8)
  !$omp unroll partial(1)
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
