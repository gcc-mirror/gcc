! { dg-do compile }
! { dg-options "-fopenmp" }

! Test that all specified forms of canonical loop bounds are accepted,
! including non-rectangular loops.

subroutine s1 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i + a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 + i, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i - a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 - i, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a1 * i, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a1 * i + a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 + a1 * i , 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a1 * i - a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 - a1 * i, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i * a1, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i * a1 + a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 + i * a1, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = i * a1 - a2, 16
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = a2 - i * a1, 16
    end do
  end do

end subroutine


subroutine s2 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i + a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 + i
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i - a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 - i
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a1 * i
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a1 * i + a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 + a1 * i 
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a1 * i - a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 - a1 * i
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i * a1
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i * a1 + a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 + i * a1
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, i * a1 - a2
    end do
  end do

  !$omp do collapse(2)
  do i = 1, 16
    do j = 1, a2 - i * a1
    end do
  end do

end subroutine

subroutine s3 (a1, a2)
  integer :: a1, a2
  integer :: i, j, k

  !$omp do collapse(3)
  do i = 1, 16
    do j = 1, i
      do k = j, 16
      end do
    end do
  end do

end subroutine
