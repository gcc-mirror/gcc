subroutine test
  implicit none
  integer :: i, j, k

  !$omp parallel do collapse(2) ordered(2) ! { dg-error {'ordered' invalid in conjunction with 'omp tile'} }
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
  !$end omp target

end subroutine test
