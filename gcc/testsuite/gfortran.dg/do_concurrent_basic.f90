! { dg-do compile }
program basic_do_concurrent
  implicit none
  integer :: i, arr(10)

  do concurrent (i = 1:10)
    arr(i) = i
  end do

  do concurrent (i=1:10);enddo
  do,concurrent (i=1:10);arr(i)=i;enddo

  print *, arr
end program basic_do_concurrent
