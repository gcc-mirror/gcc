subroutine test
  !$omp tile sizes(1,2,1) ! { dg-error "non-rectangular 'tile'" }
  do i = 1,100
    do j = 1,100
      do k = 1,i
        call dummy(i)
      end do
    end do
  end do
  !$end omp tile
end subroutine test
