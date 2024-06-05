subroutine test2
  !$omp parallel do collapse(3)
  do i=0,100
    !$omp unroll partial(2) ! { dg-error "UNROLL construct at \\\(1\\\) with PARTIAL clause generates just one loop with canonical form but 2 loops are needed" }
    do j=-300,100
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test2

subroutine test4
  !$omp parallel do collapse(3)
  do i=0,100
    !$omp tile sizes(3) ! { dg-error "TILE construct at \\\(1\\\) generates 1 loops with canonical form but 2 loops are needed" }
    do j=-300,100
      !$omp unroll partial(2)
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test4

subroutine test5
  !$omp parallel do collapse(3)
  !$omp tile sizes(3,2) ! { dg-error "TILE construct at \\\(1\\\) generates 2 loops with canonical form but 3 loops are needed" }
  do i=0,100
    do j=-300,100
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test5

subroutine test9
  !$omp parallel do collapse(3)
  do i=0,100
    !$omp tile sizes(3,3,3)
    do j=-300,100
      !$omp tile sizes(5) ! { dg-error "TILE construct at \\\(1\\\) generates 1 loops with canonical form but 2 loops are needed" }
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test9

subroutine test10
  !$omp parallel do
  do i=0,100
    !$omp tile sizes(3,3,3)
    do j=-300,100
      !$omp tile sizes(5) ! { dg-error "TILE construct at \\\(1\\\) generates 1 loops with canonical form but 2 loops are needed" }
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test10
