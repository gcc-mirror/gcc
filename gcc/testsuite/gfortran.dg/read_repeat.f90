! { dg-do run { target fd_truncate } }
! PR39528 repeated entries not read when using list-directed input.
! Test case derived from reporters example.
program rread
  implicit none
  integer :: iarr(1:7), ia, ib, i

  iarr = 0
  
  write(10,*) " 2*1  3*2 /"
  write(10,*) " 12"
  write(10,*) " 13"
  rewind(10)

  read(10,*) (iarr(i), i=1,7)
  read(10,*) ia, ib

  if (any(iarr(1:2).ne.1)) call abort
  if (any(iarr(3:5).ne.2)) call abort
  if (any(iarr(6:7).ne.0)) call abort
  if (ia .ne. 12 .or. ib .ne. 13) call abort
  
  close(10, status="delete")
end program rread
