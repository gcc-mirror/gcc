! PR middle-end/69183
! { dg-do compile }

program pr69183
  integer, allocatable :: z
  integer :: i
  !$omp do private(z)
  do i = 1, 2
    z = i
  end do
end
