! { dg-do compile }
program do_concurrent_reduce_sum
  implicit none
  integer :: i, arr(10), sum
  sum = 0

  do concurrent (i = 1:10) reduce(+:sum)
    arr(i) = i
    sum = sum + i
  end do

  print *, arr
  print *, "Sum:", sum
end program do_concurrent_reduce_sum