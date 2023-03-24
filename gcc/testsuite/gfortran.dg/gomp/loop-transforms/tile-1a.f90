
subroutine test
  !$omp tile sizes(1,2,1) ! { dg-error {not enough DO loops for \!\$OMP TILE at \(1\)} }
  do i = 1,100
     do j = 1,100
        call dummy(i)
     end do
  end do
  !$end omp tile
end subroutine test
