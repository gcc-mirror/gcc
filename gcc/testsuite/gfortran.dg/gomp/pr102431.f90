! PR middle-end/102431

program pr102431
  integer :: a(2)
  a(:) = 0
  !$omp parallel loop reduction(+:a)
  do i = 1, 8
    a = a + 1
  end do
end
