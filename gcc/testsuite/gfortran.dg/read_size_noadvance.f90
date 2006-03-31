! { dg-do run }
! PR26890 Test for use of SIZE variable in IO list.
! Test case from Paul Thomas.
! Submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>

  character(80) :: buffer, line
  integer :: nchars
  line = "The quick brown fox jumps over the lazy dog."
  open (10)
  write (10, '(a)') trim(line)
  rewind (10)
  read (10, '(a)', advance = 'no', size = nchars, eor = 998) buffer
  call abort()
998 if (nchars.ne.44) call abort()
  rewind (10)
  buffer = "how about some random text here just to be sure on this one."
  nchars = 80
  read (10, '(a)', advance = 'no', size = nchars, eor = 999) buffer(:nchars)
999 if (nchars.ne.44) call abort()
  if (buffer.ne.line) call abort()
  close (10, status="delete")
end

