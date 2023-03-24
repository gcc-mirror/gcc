function mult (a, b) result (c)
  integer, allocatable, dimension (:,:) :: a,b,c
  integer :: i, j, k, inner

  allocate(c( n, m ))

  !$omp parallel do collapse(2)
  !$omp tile sizes (8,8)
  !$omp unroll partial(2) ! { dg-error {loop nest depth after \!\$OMP UNROLL at \(1\) is insufficient for outer \!\$OMP TILE} }
  ! { dg-error {loop nest depth after \!\$OMP UNROLL at \(1\) is insufficient for outer \!\$OMP PARALLEL DO} "" { target *-*-*} .-1 }
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
  !$omp unroll partial(2) ! { dg-error {loop nest depth after \!\$OMP UNROLL at \(1\) is insufficient for outer \!\$OMP TILE} }
  do i = 1,m
     do j = 1,n
        inner = 0
        do k = 1, n
           inner = inner + a(k, i) * b(j, k)
        end do
        c(j, i) = inner
     end do
  end do

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

  !$omp parallel do collapse(2) ! { dg-error {missing canonical loop nest after \!\$OMP PARALLEL DO at \(1\)} }
  !$omp tile sizes (8,8) ! { dg-error {missing canonical loop nest after \!\$OMP TILE at \(1\)} }
  !$omp unroll full ! { dg-warning {\!\$OMP UNROLL with FULL clause at \(1\) turns loop into a non-loop} }
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
