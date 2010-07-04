! { dg-do run }
! PR43265 Followup patch for miscellaneous EOF conditions.
! Eaxamples from Tobius Burnus 
  use iso_fortran_env
  character(len=2) :: str, str2(2)
  integer :: a, b, c, ios
  str = ''
  str2 = ''

  open(99,file='test.dat',access='stream',form='unformatted', status='replace')
  write(99) ' '
  close(99)
 
  open(99,file='test.dat')
  read(99, '(T7,i2)') i
  close(99, status="delete")
  if (i /= 0) call abort

  read(str(1:0), '(T7,i1)') i
  if (i /= 0) call abort

  read(str,'(i2,/,i2)',end=111) a, b
  call abort !stop 'ERROR: Expected EOF error (1)'
  111 continue

  read(str2,'(i2,/,i2)',end=112) a, b

  read(str2,'(i2,/,i2,/,i2)',end=113) a, b, c
  call abort !stop 'ERROR: Expected EOF error (2)'

  112 call abort !stop 'ERROR: Unexpected EOF (3)'

  113 continue
  read(str,'(i2,/,i2)',end=121,pad='no') a, b
  call abort !stop 'ERROR: Expected EOF error (1)'
  121 continue

  read(str2(:),'(i2,/,i2)', end=122, pad='no') a, b
  goto 125
  122 call abort !stop 'ERROR: Expected no EOF error (2)'
  125 continue

  read(str2(:),'(i2,/,i2,/,i2)',end=123,pad='no') a, b, c
  call abort !stop 'ERROR: Expected EOF error (3)'
  123 continue

  read(str(2:1),'(i2,/,i2)',end=131, pad='no') a, b
  call abort !stop 'ERROR: Expected EOF error (1)'
  131 continue

  read(str2(:)(2:1),'(i2,/,i2)',end=132, pad='no') a, b
  call abort !stop 'ERROR: Expected EOF error (2)'
  132 continue

  read(str2(:)(2:1),'(i2,/,i2,/,i2)',end=133,pad='no') a, b, c
  call abort !stop 'ERROR: Expected EOF error (3)'
  133 continue

  read(str(2:1),'(i2,/,i2)',iostat=ios, pad='no') a, b
  if (ios /= IOSTAT_END) call abort !stop 'ERROR: expected iostat /= 0  (1)'

  read(str2(:)(2:1),'(i2,/,i2)',iostat=ios, pad='no') a, b
  if (ios /= IOSTAT_END) call  abort !stop 'ERROR: expected iostat /= 0  (2)'

  read(str2(:)(2:1),'(i2,/,i2,/,i2)',iostat=ios,pad='no') a, b, c
  if (ios /= IOSTAT_END) call abort !stop 'ERROR: expected iostat /= 0  (2)'

  ! print *, "success"
  end


