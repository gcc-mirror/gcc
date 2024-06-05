subroutine test1a
  !$omp parallel do
  !$omp tile sizes(3,3,3)
  do i=0,100
    do j=-300,100
      !$omp unroll partial(5)
      do k=-300,100
        do l=0,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test1a

subroutine test1b
  !$omp tile sizes(3,3,3)
  do i=0,100
    do j=-300,100
      !$omp unroll partial(5)
      do k=-300,100
        do l=0,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test1b
