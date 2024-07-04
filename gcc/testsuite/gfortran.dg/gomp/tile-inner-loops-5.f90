subroutine test1a
  !$omp parallel do
  !$omp tile sizes(3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5)
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test1a

subroutine test2a
  !$omp parallel do
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5,5)
      do k=-300,100
        do l=-300,100
          do m=-300,100
            call dummy (m)
          end do
        end do
      end do
    end do
  end do
end subroutine test2a

subroutine test1b
  !$omp parallel do
  !$omp tile sizes(3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5)
      do k=-300,100
        call dummy (k)
      end do
    end do
  end do
end subroutine test1b

subroutine test2b
  !$omp parallel do
  !$omp tile sizes(3,3,3,3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(5,5)
      do k=-300,100
        do l=-300,100
          do m=-300,100
            call dummy (m)
          end do
        end do
      end do
    end do
  end do
end subroutine test2b
