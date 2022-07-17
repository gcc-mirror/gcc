! { dg-do compile }
! { dg-options "-fopenmp" }

! Test that various non-canonical loops are rejected with a diagnostic.

subroutine s1 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp do collapse(2)
  do i = 1, 16
    do j = i * i, 16    ! { dg-error "not in canonical form" }
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = MAX (i, 8), 16    ! { dg-error "not in canonical form" }
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, 16, i    ! { dg-error "not in canonical form" }
    end do
  end do

  !$omp do collapse(3)
  do i = 1, 16
    do j = 1, 16
      do k = i, j    ! { dg-error "reference different iteration variables" }
      end do
    end do
  end do

  !$omp do collapse(3)
  do i = 1, 16
    do j = 1, 16
      do k = 1, i + j    !  { dg-error "not in canonical form" }
      end do
    end do
  end do

end subroutine
