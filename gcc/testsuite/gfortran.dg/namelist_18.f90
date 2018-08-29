!{ dg-do run }
!{ dg-options "-std=legacy" }
!
! Tests character delimiters for namelist write 
! provided by Paul Thomas - pault@gcc.gnu.org

program namelist_18
  character*3        ::   ch = "foo"
  character*80       ::   buffer
  namelist /mynml/ ch

  open (10, status = "scratch")
  write (10, mynml)
  rewind (10)
  read (10, '(a)', iostat = ier) buffer
  read (10, '(a)', iostat = ier) buffer
  if (ier .ne. 0) STOP 1
  close (10)
  If ((buffer(6:6) /= "f") .or. (buffer(9:9) /= """")) STOP 2 

  open (10, status = "scratch", delim ="quote")
  write (10, mynml)
  rewind (10)
  read (10, '(a)', iostat = ier) buffer
  read (10, '(a)', iostat = ier) buffer
  if (ier .ne. 0) STOP 3
  close (10)
  If ((buffer(5:5) /= """") .or. (buffer(9:9) /= """")) STOP 4

  open (10, status = "scratch", delim ="apostrophe")
  write (10, mynml)
  rewind (10)
  read (10, '(a)', iostat = ier) buffer
  read (10, '(a)', iostat = ier) buffer
  if (ier .ne. 0) STOP 5
  close (10)
  If ((buffer(5:5) /= "'") .or. (buffer(9:9) /= "'")) STOP 6

end program namelist_18
