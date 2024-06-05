subroutine test3
  !$omp tile sizes(3)
  do i=0,100
    do j=-300,100
      !$omp tile sizes(3,3)
      do k=-300,100
        do l=0,100
          call dummy (l)
        end do
      end do
    end do
  end do
end subroutine test3
