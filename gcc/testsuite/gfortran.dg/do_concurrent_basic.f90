! { dg-do run }
program basic_do_concurrent
  implicit none
  integer :: i, arr(10)

  do concurrent (i = 1:10)
    arr(i) = i
  end do

  print *, arr
end program basic_do_concurrent