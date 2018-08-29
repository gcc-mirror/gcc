!  { dg-do run }
!  { dg-options "-fconvert=big-endian" }
program main
  character (len=30) ch
  open (10,form="unformatted",convert="little_endian") 
  inquire (10, convert=ch) 
  if (ch .ne. "LITTLE_ENDIAN") STOP 1
  close (10, status="delete")

  open(11,form="unformatted")
  inquire (11, convert=ch)
  if (ch .ne. "BIG_ENDIAN") STOP 2
  close (11, status="delete")
end program main
