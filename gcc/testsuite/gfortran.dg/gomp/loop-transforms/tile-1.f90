subroutine test
  implicit none
  integer :: i, j, k

  !$omp tile sizes(1)
  do i = 1,100
     call dummy(i)
  end do

  !$omp tile sizes(1)
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(2+3)
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(-21) ! { dg-error {tile size not constant positive integer at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(0) ! { dg-error {tile size not constant positive integer at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(i) ! { dg-error {Constant expression required at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes( ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(2 ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes() ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(2,) ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(,2) ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(,i) ! { dg-error {Syntax error in 'tile sizes' list at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(i,) ! { dg-error {Constant expression required at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(1,2)
  do i = 1,100
     do j = 1,100
        call dummy(j)
     end do
  end do
  !$end omp tile

  !$omp tile sizes(1,2) ! { dg-error {not enough DO loops for \!\$OMP TILE at \(1\)} }
  do i = 1,100
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1) ! { dg-error {not enough DO loops for \!\$OMP TILE at \(1\)} }
  do i = 1,100
     do j = 1,100
        call dummy(i)
     end do
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1)
  do i = 1,100
     do j = 1,100
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1) ! { dg-error {\!\$OMP TILE inner loops must be perfectly nested at \(1\)} }
  do i = 1,100
     do j = 1,100
        do k = 1,100
           call dummy(i)
        end do
     end do
     call dummy(i)
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1) ! { dg-error {\!\$OMP TILE inner loops must be perfectly nested at \(1\)} }
  do i = 1,100
     do j = 1,100
        do k = 1,100
           call dummy(i)
        end do
        call dummy(j)
     end do
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1) ! { dg-error {\!\$OMP TILE inner loops must be perfectly nested at \(1\)} }
  do i = 1,100
     call dummy(i)
     do j = 1,100
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

  !$omp tile sizes(1,2,1) ! { dg-error {\!\$OMP TILE inner loops must be perfectly nested at \(1\)} }
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
end subroutine test
