! PR middle-end/71758

subroutine pr71758 (p)
  integer(8) :: i
  integer :: p(20)
  i = 0
  !$omp target device(i)
  !$omp end target
  !$omp target update to(p(1:1)) device(i)
end subroutine
