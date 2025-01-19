! { dg-do compile }
program do_concurrent_reduce_max
  implicit none
  integer :: i, arr(10), max_val
  max_val = 0

  do concurrent (i = 1:10) reduce(max:max_val)
    arr(i) = i * i
    max_val = max(max_val, arr(i))
  end do

  print *, arr
  print *, "Max value:", max_val
end program do_concurrent_reduce_max