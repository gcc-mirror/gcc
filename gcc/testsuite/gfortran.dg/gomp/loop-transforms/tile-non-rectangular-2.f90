subroutine test
  !$omp tile sizes(1,2,1) ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} } ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} }
  do i = 1,100
     do j = 1,100
        do k = 1,i
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test

