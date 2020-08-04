program p
   integer :: i, j, k
   real :: x
   !$omp parallel do collapse(3)
   do i = 1, 8
      do j = 1, 8
        do k = 1, 8
        end do
        x = 5  ! { dg-error "loops not perfectly nested" }
      end do
   end do
   !$omp parallel do ordered(3)
   do i = 1, 8
      do j = 1, 8
        do k = 1, 8
        end do
      end do
      x = 5  ! { dg-error "loops not perfectly nested" }
   end do
   !$omp parallel do collapse(2)  ! { dg-error "not enough DO loops for collapsed" }
   do i = 1, 8
      x = 5
      do j = 1, 8
      end do
   end do
   !$omp parallel do ordered(2)  ! { dg-error "not enough DO loops for collapsed" }
   do i = 1, 8
      x = 5
      do j = 1, 8
      end do
   end do
end
