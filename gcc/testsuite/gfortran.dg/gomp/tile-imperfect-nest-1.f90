subroutine test0
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner
  !$omp parallel do collapse(2) private(inner)
  !$omp tile sizes (8, 1)
  do i = 1,m
    !$omp tile sizes (8, 1)
    do j = 1,n
      !$omp unroll partial(10)
      do k = 1, n
        if (k == 1) then
          inner = 0
        endif
      end do
    end do
  end do
end subroutine test0
