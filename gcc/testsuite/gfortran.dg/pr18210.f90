! { dg-do run }
! Names in upper case and object names starting column 2
! Based on example provided by thomas.koenig@online.de

program pr18210

  real :: a
  character*80 :: buffer
  namelist /foo/ a

  a = 1.4
  open (10, status = "scratch")
  write (10,foo)
  rewind (10)
  read (10, '(a)') buffer
  if (buffer(2:4) /= "FOO") call abort ()
  read (10, '(a)') buffer
  if (buffer(1:2) /= " A") call abort ()
  close (10)

end program pr18210
