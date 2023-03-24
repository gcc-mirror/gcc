
subroutine test1
  implicit none
  integer :: i, j, k

  !$omp tile sizes (1,2)
  !$omp tile sizes (1)  ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TILE} }
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

end subroutine test1

subroutine test2
  implicit none
  integer :: i, j, k

  !$omp tile sizes (1,2)
  !$omp tile sizes (1)  ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TILE} }
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

end subroutine test2

subroutine test3
  implicit none
  integer :: i, j, k

  !$omp target teams distribute
  !$omp tile sizes (1,2)
  !$omp tile sizes (1)  ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TILE} }
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

end subroutine test3

subroutine test4
  implicit none
  integer :: i, j, k

  !$omp target teams distribute collapse(2)
  !$omp tile sizes (8)  ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TARGET TEAMS DISTRIBUTE} }
  !$omp tile sizes (1,2)
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile

end subroutine test4

subroutine test5
  implicit none
  integer :: i, j, k

  !$omp parallel do collapse(2) ordered(2)
  !$omp tile sizes (8)  ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP PARALLEL DO} }
  !$omp tile sizes (1,2)
  do i = 1,100
     do j = 1,100
        call dummy(j)
        do k = 1,100
           call dummy(i)
        end do
     end do
  end do
  !$end omp tile
  !$end omp tile
  !$end omp target

end subroutine test5
