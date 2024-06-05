subroutine test1
  !$omp parallel do collapse(2)
  do i=0,100
    !$omp unroll partial(2)
    do j=-300,100
      call dummy (j)
    end do
  end do
end subroutine test1

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
