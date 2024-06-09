subroutine test
  implicit none

  integer :: i, j, k
  !$omp target parallel do collapse(2)
  do i = -300, 100
    !$omp unroll partial
    do j = i,i*2 ! { dg-message "Non-rectangular loops from generated loops unsupported" }
      call dummy (i)
    end do
  end do

  !$omp target parallel do collapse(3)
  do i = -300, 100
    do j = 1,10
      !$omp unroll partial
      do k = j,j*2 + 1 ! { dg-message "Non-rectangular loops from generated loops unsupported" }
        call dummy (i)
      end do
    end do
  end do
end subroutine
