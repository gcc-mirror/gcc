! { dg-do compile }
program do_concurrent_multiple_reduce
  implicit none
  integer :: i, arr(10), sum, product
  sum = 0
  product = 1

  do concurrent (i = 1:10) reduce(+:sum) reduce(*:product)
    arr(i) = i
    sum = sum + i
    product = product * i
  end do

  print *, arr
  print *, "Sum:", sum
  print *, "Product:", product
end program do_concurrent_multiple_reduce