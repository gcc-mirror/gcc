! { dg-do compile }
program nested_do_concurrent
  implicit none
  integer :: i, j, x(10, 10)
  integer :: total_sum

  total_sum = 0

  ! Outer loop remains DO CONCURRENT
  do concurrent (i = 1:10)
    ! Inner loop changed to regular DO loop
    do j = 1, 10
      x(i, j) = i * j
    end do
  end do

  ! Separate loops for summation
  do i = 1, 10
    do j = 1, 10
      total_sum = total_sum + x(i, j)
    end do
  end do

  print *, "Total sum:", total_sum
  print *, "Array:", x
end program nested_do_concurrent