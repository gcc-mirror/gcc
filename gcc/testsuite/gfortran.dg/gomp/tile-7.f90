subroutine test
  implicit none
  integer :: i, j, k

  !$omp tile sizes(-21) ! { dg-error "INTEGER expression of SIZES clause at \\\(1\\\) must be positive" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(0) ! { dg-error "INTEGER expression of SIZES clause at \\\(1\\\) must be positive" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(i) ! { dg-error "SIZES requires constant expression" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes ! { dg-error "Expected '\\\(' after 'sizes' at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes( ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(2 ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes() ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(2,) ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(,2) ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(,i) ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(i,) ! { dg-error "Syntax error in OpenMP expression list at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile ! { dg-error "Unexpected !\\\$OMP END TILE statement at \\\(1\\\)" }

  !$omp tile sizes(1,2) ! { dg-error "not enough DO loops for collapsed !\\\$OMP TILE" }
  do i = 1,100
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1) ! { dg-error "not enough DO loops for collapsed !\\\$OMP TILE" }
  do i = 1,100
    do j = 1,100
      call dummy(i)
    end do
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested at \\\(1\\\)" }
  do i = 1,100
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
    end do
    call dummy(i)
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested at \\\(1\\\)" }
  do i = 1,100
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
      call dummy(j)
    end do
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested at \\\(1\\\)" }
  do i = 1,100
    call dummy(i)
    do j = 1,100
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile

  !$omp tile sizes(1,2,1) ! { dg-error "!\\\$OMP TILE inner loops must be perfectly nested at \\\(1\\\)" }
  do i = 1,100
    do j = 1,100
      call dummy(j)
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
end subroutine test
