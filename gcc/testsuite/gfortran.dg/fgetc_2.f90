! Testcase for the FGETC and FPUTC intrinsics
! { dg-do run }
  character(len=5) s
  integer st

  s = "12345"
  open(10,status="scratch")
  write(10,"(A)") "abcde"
  rewind(10)
  st = fgetc(10,s)
  if ((st /= 0) .or. (s /= "a    ")) call abort
  st = fgetc(10,s)
  close(10)

  open(10,status="scratch")
  s = "12345"
  st = fputc(10,s)
  if (st /= 0) call abort
  st = fputc(10,"2")
  if (st /= 0) call abort
  st = fputc(10,"3 ")
  if (st /= 0) call abort
  rewind(10)
  st = fgetc(10,s)
  if (s(1:1) /= "1") call abort
  st = fgetc(10,s)
  if (s(1:1) /= "2") call abort
  st = fgetc(10,s)
  if ((s(1:1) /= "3") .or. (st /= 0)) call abort
  st = fgetc(10,s)
  if (st /= -1) call abort
  close (10)

! FGETC and FPUTC on units not opened should not work
  st = fgetc(12,s)
  if (st /= -1) call abort
  st = fputc(12,s)
  if (st /= -1) call abort
  end
