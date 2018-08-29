! { dg-do run }
  integer(kind=8) o
  open (10, status="scratch")
  if (ftell(10) /= 0) STOP 1
  write (10,"(A)") "1234567"
  if (ftell(10) /= 8 .and. ftell(10) /= 9) STOP 2
  o = ftell(10)
  write (10,"(A)") "1234567"
  if (ftell(10) /= 2 * o) STOP 3
  close (10)
  if (ftell(10) /= -1) STOP 4
  end
