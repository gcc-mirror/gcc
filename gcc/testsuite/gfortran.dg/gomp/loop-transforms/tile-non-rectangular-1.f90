subroutine test1
  !$omp tile sizes(1)
  do i = 1,100
     do j = 1,i
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test1

subroutine test2
  !$omp tile sizes(1,2) ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} }
  do i = 1,100
     do j = 1,i
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test2

subroutine test3
  !$omp tile sizes(1,2,1) ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} }
  do i = 1,100
     do j = 1,i
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test3

subroutine test4
  !$omp tile sizes(1,2,1) ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} }
  do i = 1,100
     do j = 1,100
        do k = 1,i
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test4

subroutine test5
  !$omp tile sizes(1,2)
  do i = 1,100
     do j = 1,100
        do k = 1,j
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test5

subroutine test6
  !$omp tile sizes(1,2,1) ! { dg-error {'tile' loop transformation may not appear on non-rectangular for} }
  do i = 1,100
     do j = 1,100
        do k = 1,j
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test6
