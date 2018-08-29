! { dg-do run }
! PR 29627 - partial reads of unformatted records
program main
  character a(3)
  character(len=50) msg
  open(10, form="unformatted", status="unknown")
  write (10) 'a'
  write (10) 'c'
  a = 'b'
  rewind 10
  read (10, err=20, iomsg=msg) a
  STOP 1
20 continue
  if (msg .ne. "I/O past end of record on unformatted file") STOP 2
  if (a(1) .ne. 'a' .or. a(2) .ne. 'b' .or. a(3) .ne. 'b') STOP 3
  close (10, status="delete")
end program main
