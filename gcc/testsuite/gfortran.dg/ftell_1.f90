! { dg-do run }
  integer*8 o

  open (10, status="scratch")
  call ftell (10, o)
  if (o /= 0) call abort
  write (10,"(A)") "1234567"
  call ftell (10, o)
  if (o /= 8) call abort
  close (10)
  call ftell (10, o)
  if (o /= -1) call abort
  end
