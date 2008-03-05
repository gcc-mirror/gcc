! { dg-do run { target fd_truncate } }
! PR33307 I/O read/positioning problem - in BACKSPACE
! Test case devloped from test in PR by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program gfcbug69b
  ! Modified example program
  implicit none
  integer, parameter :: iunit = 63
  integer            :: istat, k, ios
  character(len=20) :: line, message

  open (iunit)
  write (iunit, '(a)') "! ***Remove this line***"
  write (iunit, '(a)') "&FOO        file='foo' /"
  write (iunit, '(a)', advance="no") "&BAR        file='bar' /"
  close (iunit)
! Note: Failure occurred only when ACTION="read" was specified
  open (iunit, action="read", status="old")
  
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) backspace (iunit)
  rewind (iunit)
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= 0) call abort
  read (iunit,'(a)',iostat=ios) line
  if (ios /= -1) call abort
  close (iunit, status="delete")
end program gfcbug69b
