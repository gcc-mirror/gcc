subroutine test0m
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner
  !$omp parallel do collapse(2) private(inner)
  do i = 1,m
    !$omp tile sizes (8, 1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested" }
    do j = 1,n
      do k = 1, n
        if (k == 1) then
          inner = 0
        endif
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end subroutine test0m

subroutine test1
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner
  !$omp parallel do collapse(2) private(inner)
  !$omp tile sizes (8, 1)
  do i = 1,m
    !$omp tile sizes (8, 1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested" }
    do j = 1,n
      !$omp unroll partial(10)
      do k = 1, n
        if (k == 1) then
          inner = 0
        endif
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end subroutine test1

subroutine test2
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner
  !$omp parallel do collapse(2) private(inner)
  !$omp tile sizes (8, 1)
  do i = 1,m
    !$omp tile sizes (8, 1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested" }
    do j = 1,n
      do k = 1, n
        if (k == 1) then
          inner = 0
        endif
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end subroutine test2

subroutine test3
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner
  !$omp parallel do collapse(2) private(inner)
  do i = 1,m
    !$omp tile sizes (8, 1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested" }
    do j = 1,n
      do k = 1, n
        if (k == 1) then
          inner = 0
        endif
        inner = inner + a(k, i) * b(j, k)
      end do
      c(j, i) = inner
    end do
  end do
end subroutine test3
