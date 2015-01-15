! { dg-do compile }

program reduction
  integer, parameter    :: n = 40, c = 10
  integer               :: i, sum

  call redsub (sum, n, c)
end program reduction

subroutine redsub(sum, n, c)
  integer :: sum, n, c

  sum = 0

  !$acc parallel vector_length(n) copyin (n, c)
  !$acc loop reduction(+:sum)
  do i = 1, n
     sum = sum + c
  end do
  !$acc end parallel
end subroutine redsub
