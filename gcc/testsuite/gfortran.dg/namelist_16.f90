!{ dg-do run { target fd_truncate } }
! Tests namelist on complex variables
! provided by Paul Thomas - pault@gcc.gnu.org
program namelist_16
  complex(kind=8), dimension(2)  ::   z
  namelist /mynml/ z
  z = (/(1.0,2.0), (3.0,4.0)/)

  open (10, status = "scratch")
  write (10, '(A)') "&mynml z(1)=(5.,6.) z(2)=(7.,8.) /"
  rewind (10)

  read (10, mynml, iostat = ier)
  if (ier .ne. 0) call abort ()
  close (10)

  open (10, status = "scratch")
  write (10, mynml, iostat = ier)
  if (ier .ne. 0) call abort ()
  rewind (10)

  z = (/(1.0,2.0), (3.0,4.0)/)
  read (10, mynml, iostat = ier)
  if (ier .ne. 0) call abort ()
  close (10)

  if ((z(1) .ne. (5.0,6.0)) .or. (z(2) .ne. (7.0,8.0))) call abort ()

end program namelist_16 
