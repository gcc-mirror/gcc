! { dg-do run }
! { dg-options "-std=legacy" }
!
! Check that rewind doesn't delete a file.
! Writing to the file truncates it at the end of the current record.  Out
! IO library was defering the actual truncation until the file was rewound.
! A second rewind would then (incorrectly) think the file had just been
! written to, and truncate the file to zero length.
program foo
  character*11 s
  open(unit=11, status="SCRATCH")
  write(11, '(a11)') "Hello World"
  rewind(11)
  rewind(11)
  s = ""
  read(11, '(a11)') s
  close(11)
  if (s .ne. "Hello World") call abort
end program

