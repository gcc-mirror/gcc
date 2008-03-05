! { dg-do run { target fd_truncate } }
! Program to test for proper EOF errors when reading past the end of a file.
! We used to get this wrong when a formatted read followed a list formatted
! read.
program eof_1
  character(len=5) :: s

  open (unit=11, status="SCRATCH")
  write (11, '(a)') "Hello"
  rewind(11)
  read(11, *) s
  if (s .ne. "Hello") call abort
  read(11, '(a5)', end=10) s
  call abort
10 continue
  close (11)
end

