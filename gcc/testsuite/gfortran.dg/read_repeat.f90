! { dg-do run }
! PR39528 repeated entries not read when using list-directed input.
! Test case derived from reporters example.
program rread
  implicit none
  integer :: iarr(1:7), ia, ib, i

  iarr = 0
  
  open(10, status="scratch")
  write(10,*) " 2*1  3*2 /"
  write(10,*) " 12"
  write(10,*) " 13"
  rewind(10)

  read(10,*) (iarr(i), i=1,7)
  read(10,*) ia, ib

  if (any(iarr(1:2).ne.1)) STOP 1
  if (any(iarr(3:5).ne.2)) STOP 2
  if (any(iarr(6:7).ne.0)) STOP 3
  if (ia .ne. 12 .or. ib .ne. 13) STOP 4
  
  close(10)
end program rread
