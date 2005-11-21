! PR fortran/24774
! { dg-do run }
  logical :: l
  l = .true.
  inquire (file='inquire_9 file that should not exist', exist=l)
  if (l) call abort
  l = .true.
  inquire (unit=-16, exist=l)
  if (l) call abort
  open (unit=16, file='inquire_9.tst')
  print (unit=16, fmt='(a)'), 'Test'
  l = .false.
  inquire (unit=16, exist=l)
  if (.not.l) call abort
  l = .false.
  inquire (file='inquire_9.tst', exist=l)
  if (.not.l) call abort
  close (unit=16)
  l = .false.
  inquire (file='inquire_9.tst', exist=l)
  if (.not.l) call abort
  open (unit=16, file='inquire_9.tst')
  close (unit=16, status='delete')
end
