! PR middle-end/51089
! { dg-do compile }
! { dg-options "-O -fexceptions -fopenmp" }

subroutine foo
  real, allocatable, dimension(:) :: s
  real, dimension(:, :, :), pointer :: t
  call fn1 (t, s)
  call fn2 ()
end subroutine foo
subroutine bar
  integer :: i
!$omp parallel do
  do i = 1, 10
  end do
end subroutine bar
