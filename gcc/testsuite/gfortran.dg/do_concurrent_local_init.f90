! { dg-do compile }
! { dg-options "-fmax-errors=1" }
program do_concurrent_local_init
  implicit none
  integer :: i, arr(10), temp
  do concurrent (i = 1:10) local_init(temp)  ! { dg-error "LOCAL and LOCAL_INIT are not yet supported for 'do concurrent' constructs" }
    temp = i
    arr(i) = temp
  end do
  print *, arr
end program do_concurrent_local_init