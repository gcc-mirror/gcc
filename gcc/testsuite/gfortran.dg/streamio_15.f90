! { dg-do run { target fd_truncate } }
! PR35132 Formatted stream I/O write should truncate.
! Test case adapted from PR by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program main
  implicit none
  character(len=6) :: c
  integer :: i, newline_length

  open(20,status="scratch",access="stream",form="formatted")
  write(20,"()")
  inquire(20,pos=newline_length)
  newline_length = newline_length - 1
  if (newline_length < 1 .or. newline_length > 2) call abort
  close(20)

  open(20,file="foo.txt",form="formatted",access="stream")
  write(20,'(A)') '123456'
  write(20,'(A)') 'abcdef'
  write(20,'(A)') 'qwerty'
  rewind 20
  ! Skip over the first line
  read(20,'(A)') c
  if (c.ne.'123456') call abort
  ! Save the position
  inquire(20,pos=i)
  if (i.ne.7+newline_length) call abort
  ! Read in the complete line...
  read(20,'(A)') c
  if (c.ne.'abcdef') call abort
  ! Write out the first four characters
  write(20,'(A)',pos=i,advance="no") 'ASDF'
  ! Fill up the rest of the line.  Here, we know the length.  If we
  ! don't, things will be a bit more complicated.
  write(20,'(A)') c(5:6)
  ! Copy the file to standard output
  rewind 20
  c = ""
  read(20,'(A)') c
  if (c.ne.'123456') call abort
  read(20,'(A)') c
  if (c.ne.'ASDFef') call abort
  read(20,'(A)', iostat=i) c
  if (i /= -1) call abort
  close (20, status="delete")
end program main
