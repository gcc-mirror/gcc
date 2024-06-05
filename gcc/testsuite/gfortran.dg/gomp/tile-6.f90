subroutine test
  !$omp tile sizes(1,2,1) ! { dg-error "not enough DO loops for collapsed !\\\$OMP TILE" }
  do i = 1,100
    do j = 1,100
      call dummy(i)
    end do
  end do
  !$omp end tile
end subroutine test
