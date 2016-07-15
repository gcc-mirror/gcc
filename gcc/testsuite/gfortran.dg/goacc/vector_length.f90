program t
  implicit none
  integer, parameter :: n = 100
  integer a(n), i

  !$acc parallel loop num_gangs(100) num_workers(1) vector_length(32)
  do i = 1, n
     a(i) = i
  enddo
  !$acc end parallel loop
end program t
