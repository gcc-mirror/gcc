subroutine test
  implicit none
  integer :: i, j, k

  !$omp parallel do collapse(2) ordered(2) ! { dg-error "'ordered' clause used with generated loops" }
  !$omp tile sizes (1,2)
  do i = 1,100
    do j = 1,100
      call dummy(j)
      do k = 1,100
        call dummy(i)
      end do
    end do
  end do
  !$omp end tile
  !$omp end parallel do
end subroutine test
