! PR fortran/78298
! { dg-do compile }
! { dg-additional-options "-O2" }

program pr78298
  integer :: i, j, n
  n = 2
  !$omp parallel
  !$omp do
  do i = 1, n
    !$omp parallel
    !$omp do
    do j = 1, n
      call sub(i)
    end do
    !$omp end parallel
  end do
  !$omp end parallel
  !call unused()
contains
  subroutine sub(x)
    integer :: x
  end
  subroutine unused()
    i = 0
    j = 0
  end
end
