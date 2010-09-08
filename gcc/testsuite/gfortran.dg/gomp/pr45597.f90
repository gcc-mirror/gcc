! PR fortran/45597
! { dg-do compile }
! { dg-options "-fopenmp" }

subroutine foo(n)
  integer :: i, n(6)
  !$omp parallel do default(none) shared(n)
    do i = 1, 6
      if (n(i).gt.0) cycle
    end do
end subroutine
subroutine bar(n)
  integer :: i, j, k, n(6, 6, 6)
  !$omp parallel do default(none) shared(n) collapse(3)
    do i = 1, 6
      do j = 1, 6
        do k = 1, 6
          if (n(i, j, k).gt.0) cycle
        end do
      end do
    end do
end subroutine
