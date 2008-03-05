! { dg-do run { target fd_truncate } }
  integer(kind=8) o
  open (10, status="scratch")
  if (ftell(10) /= 0) call abort
  write (10,"(A)") "1234567"
  if (ftell(10) /= 8 .and. ftell(10) /= 9) call abort
  o = ftell(10)
  write (10,"(A)") "1234567"
  if (ftell(10) /= 2 * o) call abort
  close (10)
  if (ftell(10) /= -1) call abort
  end
