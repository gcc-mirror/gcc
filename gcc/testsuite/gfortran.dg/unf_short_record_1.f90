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
  call abort
20 continue
  if (msg .ne. "Short record on unformatted read") call abort
  if (a(1) .ne. 'a' .or. a(2) .ne. 'b' .or. a(3) .ne. 'b') call abort
  close (10, status="delete")
end program main
