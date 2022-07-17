! PR fortran/85313
! { dg-do compile }

!$omp do collapse(3)
  do i = 1, 10
    do j = i, 20
      do k = 1, 2
      end do
    end do
  end do
!$omp do collapse(3)
  do i = 1, 10
    do j = 1, 5
      do k = i, 20
      end do
    end do
  end do
!$omp do collapse(3)
  do i = 1, 10
    do j = 1, 5
      do k = j, 20
      end do
    end do
  end do
end
