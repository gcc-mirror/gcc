! Testcase for the FGETC and FPUTC intrinsics
! { dg-do compile }
  character(len=5) s
  integer st

  s = "12345"
  open(status="scratch")
  write(*,"(A)") "abcde"
  rewind(10)
  st = fget(s)
  if ((st /= 0) .or. (s /= "a    ")) call abort
  st = fget(s)
  close(10)

  open(status="scratch")
  s = "12345"
  st = fput(s)
  if (st /= 0) call abort
  st = fput("2")
  if (st /= 0) call abort
  st = fput("3 ")
  if (st /= 0) call abort
  rewind(10)
  st = fget(s)
  if (s(1:1) /= "1") call abort
  st = fget(s)
  if (s(1:1) /= "2") call abort
  st = fget(s)
  if ((s(1:1) /= "3") .or. (st /= 0)) call abort
  st = fget(s)
  if (st /= -1) call abort
  close (10)

  end
