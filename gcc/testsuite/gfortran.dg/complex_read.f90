! { dg-do run { target fd_truncate } }
! Test of the fix to the bug in NIST fm906.for.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program complex_read
  complex            ::  a
  open (10, status="scratch")

! Test that we have not broken the one line form.

  write (10, *) " ( 0.99  ,  9.9  )"
  rewind (10)
  read (10,*) a
  if (a.ne.(0.99, 9.90)) STOP 1

! Test a new record after the.comma (the original bug).

  rewind (10)
  write (10, *) " ( 99.0   ,"
  write (10, *) "   999.0  )"
  rewind (10)
  read (10,*) a
  if (a.ne.(99.0, 999.0)) STOP 2

! Test a new record before the.comma 

  rewind (10)
  write (10, *) " ( 0.99  "
  write (10, *) " , 9.9  )"
  rewind (10)
  read (10,*) a
  if (a.ne.(0.99, 9.90)) STOP 3

! Test a new records before and after the.comma 

  rewind (10)
  write (10, *) " ( 99.0   "
  write (10, *) ",         "
  write (10, *) "  999.0  )"
  rewind (10)
  read (10,*) a
  if (a.ne.(99.0, 999.0)) STOP 4

! Test a new records and blank records before and after the.comma 

  rewind (10)
  write (10, *) " ( 0.99   "
  write (10, *) "          "
  write (10, *) ",         "
  write (10, *) "          "
  write (10, *) "  9.9    )"
  rewind (10)
  read (10,*) a
  if (a.ne.(0.99, 9.9)) STOP 5

  close (10)
end program complex_read

