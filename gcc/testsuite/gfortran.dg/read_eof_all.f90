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
  if (i /= 0) STOP 1

  read(str(1:0), '(T7,i1)') i
  if (i /= 0) STOP 2

  read(str,'(i2,/,i2)',end=111) a, b
  STOP 3!stop 'ERROR: Expected EOF error (1)'
  111 continue

  read(str2,'(i2,/,i2)',end=112) a, b

  read(str2,'(i2,/,i2,/,i2)',end=113) a, b, c
  STOP 4!stop 'ERROR: Expected EOF error (2)'

  112 STOP 5!stop 'ERROR: Unexpected EOF (3)'

  113 continue
  read(str,'(i2,/,i2)',end=121,pad='no') a, b
  STOP 6!stop 'ERROR: Expected EOF error (1)'
  121 continue

  read(str2(:),'(i2,/,i2)', end=122, pad='no') a, b
  goto 125
  122 STOP 7!stop 'ERROR: Expected no EOF error (2)'
  125 continue

  read(str2(:),'(i2,/,i2,/,i2)',end=123,pad='no') a, b, c
  STOP 8!stop 'ERROR: Expected EOF error (3)'
  123 continue

  read(str(2:1),'(i2,/,i2)',end=131, pad='no') a, b
  STOP 9!stop 'ERROR: Expected EOF error (1)'
  131 continue

  read(str2(:)(2:1),'(i2,/,i2)',end=132, pad='no') a, b
  STOP 10!stop 'ERROR: Expected EOF error (2)'
  132 continue

  read(str2(:)(2:1),'(i2,/,i2,/,i2)',end=133,pad='no') a, b, c
  STOP 11!stop 'ERROR: Expected EOF error (3)'
  133 continue

  read(str(2:1),'(i2,/,i2)',iostat=ios, pad='no') a, b
  if (ios /= IOSTAT_END) STOP 12!stop 'ERROR: expected iostat /= 0  (1)'

  read(str2(:)(2:1),'(i2,/,i2)',iostat=ios, pad='no') a, b
  if (ios /= IOSTAT_END) STOP 13!stop 'ERROR: expected iostat /= 0  (2)'

  read(str2(:)(2:1),'(i2,/,i2,/,i2)',iostat=ios,pad='no') a, b, c
  if (ios /= IOSTAT_END) STOP 14!stop 'ERROR: expected iostat /= 0  (2)'

  ! print *, "success"
  end


