! PR middle-end/43337
! { dg-do compile }
! { dg-options "-fopenmp -O2 -g" }

subroutine pr43337
  integer :: a, b(10)
  call foo (b)
  call bar (b)
contains
  subroutine foo (b)
    integer :: b(10)
!$omp parallel if (.false.)
!$omp task if (.false.) shared(b)
    do a = 1, 10
      b(a) = 1
    end do
!$omp end task
!$omp end parallel
  end subroutine foo
  subroutine bar (b)
    integer :: b(10)
!$omp parallel if (.false.)
!$omp parallel if (.false.)
    do a = 1, 10
      b(a) = 1
    end do
!$omp end parallel
!$omp end parallel
  end subroutine bar
end subroutine pr43337
