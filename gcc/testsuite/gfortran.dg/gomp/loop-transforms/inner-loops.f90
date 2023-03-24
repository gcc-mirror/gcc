subroutine test1
  !$omp parallel do collapse(2)
  do i=0,100
     !$omp unroll partial(2)
     do j=-300,100
        call dummy (j)
     end do
  end do
end subroutine test1

subroutine test2
  !$omp parallel do collapse(3)
  do i=0,100
     !$omp unroll partial(2) ! { dg-error {loop nest depth after \!\$OMP UNROLL at \(1\) is insufficient for outer \!\$OMP PARALLEL DO} }
     do j=-300,100
        do k=-300,100
           call dummy (k)
        end do
    end do
   end do
end subroutine test2

subroutine test3
!$omp parallel do collapse(3)
do i=0,100
    do j=-300,100
    !$omp unroll partial(2)
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test3

subroutine test4
!$omp parallel do collapse(3)
do i=0,100
   !$omp tile sizes(3) ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP PARALLEL DO} }
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
  !$omp tile sizes(3,2) ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP PARALLEL DO} }
  do i=0,100
     do j=-300,100
        do k=-300,100
           call dummy (k)
        end do
     end do
  end do
end subroutine test5

subroutine test6
!$omp parallel do collapse(3)
do i=0,100
   !$omp tile sizes(3,2)
    do j=-300,100
    !$omp unroll partial(2)
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test6

subroutine test7
!$omp parallel do collapse(3)
do i=0,100
   !$omp tile sizes(3,3)
    do j=-300,100
    !$omp tile sizes(5)
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test7

subroutine test8
!$omp parallel do collapse(1)
do i=0,100
   !$omp tile sizes(3,3)
    do j=-300,100
    !$omp tile sizes(5)
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test8

subroutine test9
!$omp parallel do collapse(3)
do i=0,100
   !$omp tile sizes(3,3,3) ! { dg-error {not enough DO loops for \!\$OMP TILE at \(1\)} }
    do j=-300,100
    !$omp tile sizes(5) ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TILE} }
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test9

subroutine test10
!$omp parallel do
do i=0,100
   !$omp tile sizes(3,3,3) ! { dg-error {not enough DO loops for \!\$OMP TILE at \(1\)} }
    do j=-300,100
    !$omp tile sizes(5) ! { dg-error {loop nest depth after \!\$OMP TILE at \(1\) is insufficient for outer \!\$OMP TILE} }
    do k=-300,100
        call dummy (k)
    end do
end do
end do
end subroutine test10

