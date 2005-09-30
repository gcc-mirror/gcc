! { dg-do run }
! Test reopening with io status='old'
program iostatus
  open (1, file='foo', status='replace') ! Make sure file exists.
  open (1, file='foo', status='old')
  open (1, file='foo', status='old')
  close (1, status='delete')
end program iostatus
