subroutine test2a
  !$omp parallel do
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp unroll partial(5)  ! { dg-error "UNROLL construct at \\\(1\\\) with PARTIAL clause generates just one loop with canonical form but 2 loops are needed" }
      do k=-300,100
        do l=0,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test2a

subroutine test2b
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp unroll partial(5)  ! { dg-error "UNROLL construct at \\\(1\\\) with PARTIAL clause generates just one loop with canonical form but 2 loops are needed" }
      do k=-300,100
        do l=0,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test2b
