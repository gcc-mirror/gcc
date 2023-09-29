program p
   integer :: i, j, k
   real :: x
   !$omp parallel do collapse(3)
   do i = 1, 8
      do j = 1, 8
        do k = 1, 8
        end do
        x = 5
      end do
   end do
   !$omp parallel do ordered(3) ! { dg-error "inner loops must be perfectly nested" }
   do i = 1, 8
      do j = 1, 8
        do k = 1, 8
        end do
      end do
      x = 5
   end do
   !$omp parallel do collapse(2)
   do i = 1, 8
      x = 5
      do j = 1, 8
      end do
   end do
   !$omp parallel do ordered(2) ! { dg-error "inner loops must be perfectly nested" }
   do i = 1, 8
      x = 5
      do j = 1, 8
      end do
   end do
end
