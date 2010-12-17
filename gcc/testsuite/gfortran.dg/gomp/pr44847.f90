! PR fortran/44847
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine pr44847_1
  integer :: i, j
!$omp parallel do collapse(2)
l:do i = 1, 2
    do j = 1, 2
      cycle l		! { dg-error "CYCLE statement" }
    end do
  end do l
end subroutine
subroutine pr44847_2
  integer :: i, j, k
!$omp parallel do collapse(3)
  do i = 1, 2
  l:do j = 1, 2
      do k = 1, 2
        cycle l		! { dg-error "CYCLE statement" }
      end do
    end do l
  end do
end subroutine
subroutine pr44847_3
  integer :: i, j
!$omp parallel do
l:do i = 1, 2
    do j = 1, 2
      cycle l
    end do
  end do l
end subroutine
subroutine pr44847_4
  integer :: i, j, k
!$omp parallel do collapse(2)
  do i = 1, 2
  l:do j = 1, 2
      do k = 1, 2
        cycle l
      end do
    end do l
  end do
end subroutine
subroutine pr44847_5
  integer :: i, j
!$omp parallel do collapse(2)
l:do i = 1, 2
    do j = 1, 2
      exit l		! { dg-error "EXIT statement" }
    end do
  end do l
end subroutine
subroutine pr44847_6
  integer :: i, j, k
!$omp parallel do collapse(3)
  do i = 1, 2
  l:do j = 1, 2
      do k = 1, 2
        exit l		! { dg-error "EXIT statement" }
      end do
    end do l
  end do
end subroutine
subroutine pr44847_7
  integer :: i, j, k
!$omp parallel do collapse(2)
  do i = 1, 2
  l:do j = 1, 2
      do k = 1, 2
        exit l		! { dg-error "EXIT statement" }
      end do
    end do l
  end do
end subroutine
subroutine pr44847_8
  integer :: i, j, k
!$omp parallel do
  do i = 1, 2
  l:do j = 1, 2
      do k = 1, 2
        exit l
      end do
    end do l
  end do
end subroutine
