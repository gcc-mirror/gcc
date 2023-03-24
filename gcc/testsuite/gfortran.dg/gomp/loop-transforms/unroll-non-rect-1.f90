subroutine test
  implicit none

  integer :: i, j, k
  !$omp target parallel do collapse(2) ! { dg-error {invalid OpenMP non-rectangular loop step; '\(2 - 1\) \* 1' is not a multiple of loop 2 step '5'} }
  do i = -300, 100
    !$omp unroll partial
    do j = i,i*2
      call dummy (i)
    end do
   end do

  !$omp target parallel do collapse(3) ! { dg-error {invalid OpenMP non-rectangular loop step; '\(2 - 1\) \* 1' is not a multiple of loop 3 step '5'} }
  do i = -300, 100
    do j = 1,10
       !$omp unroll partial
       do k = j,j*2 + 1
      call dummy (i)
    end do
   end do
  end do

  !$omp unroll full
  do i = -3, 5
    do j = 1,10
       do k = j,j*2 + 1
      call dummy (i)
    end do
   end do
  end do
end subroutine
