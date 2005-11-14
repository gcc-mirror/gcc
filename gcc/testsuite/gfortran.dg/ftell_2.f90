! { dg-do run }
  open (10, status="scratch")
  if (ftell(10) /= 0) call abort
  write (10,"(A)") "1234567"
  if (ftell(10) /= 8) call abort
  close (10)
  if (ftell(10) /= -1) call abort
  end
