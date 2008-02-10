! { dg-do run }
! Test that we can write an unformatted stream file without
! truncating.
program main
  character (len=10) c
  open(10, form="unformatted", access="stream", position="rewind")
  write (10) '1234567890abcde'
  c = ''
  read (10,pos=1) c
  if (c /= '1234567890') call abort
  c = ''
  read (10,pos=6) c
  if (c /= '67890abcde') call abort
  write (10,pos=3) 'AB'
  c = ''
  read (10,pos=1) c
  if (c /= '12AB567890') call abort
  c = ''
  read (10,pos=6) c
  if (c /= '67890abcde') call abort
  close (10,status="delete")
end program main
