! { dg-do run }
! Check that we can open more than 26 scratch files concurrently
  integer :: i
  do i = 1, 30
    print *, i
    open(100+i,status="scratch")
  end do
end
