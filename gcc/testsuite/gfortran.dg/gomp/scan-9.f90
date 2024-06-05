subroutine foo (c, d, a)
  integer :: i, a, c(64), d(64)
  !$omp do reduction (inscan, +: a)
  !$omp tile sizes (2)
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a) ! { dg-error "Unexpected !\\\$OMP SCAN at \\\(1\\\) outside loop construct with 'inscan' REDUCTION clause" }
    d(i) = a
  end do
end subroutine foo

subroutine bar (c, d, a)
  integer :: i, j, a, c(64, 64), d(64, 64)
  !$omp do collapse (2) reduction (inscan, +: a)
  do i = 1, 64
    !$omp tile sizes (2)
    do j = 1, 64
      d(i, j) = a
      !$omp scan exclusive (a) ! { dg-error "Unexpected !\\\$OMP SCAN at \\\(1\\\) outside loop construct with 'inscan' REDUCTION clause" }
      a = a + c(i, j)
    end do
  end do
end subroutine bar

subroutine baz (c, d, a)
  integer :: i, a, c(64), d(64)
  !$omp do reduction (inscan, +: a)
  !$omp unroll partial (2)
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a) ! { dg-error "Unexpected !\\\$OMP SCAN at \\\(1\\\) outside loop construct with 'inscan' REDUCTION clause" }
    a = a + c(i)
  end do
end subroutine baz

subroutine qux (c, d, a)
  integer :: i, j, a, c(64, 64), d(64, 64)
  !$omp do collapse (2) reduction (inscan, +: a)
  do i = 1, 64
    !$omp tile sizes (2)
    do j = 1, 64
      a = a + c(i, j)
      !$omp scan inclusive (a) ! { dg-error "Unexpected !\\\$OMP SCAN at \\\(1\\\) outside loop construct with 'inscan' REDUCTION clause" }
      d(i, j) = a
    end do
  end do
end subroutine qux
