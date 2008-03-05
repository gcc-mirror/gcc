! Testcase for the FGETC and FPUTC intrinsics
! { dg-do run { target fd_truncate } }
  character(len=5) s
  integer st

  s = "12345"
  open(10,status="scratch")
  write(10,"(A)") "abcde"
  rewind(10)
  call fgetc(10,s,st)
  if ((st /= 0) .or. (s /= "a    ")) call abort
  call fgetc(10,s,st)
  close(10)

  open(10,status="scratch")
  s = "12345"
  call fputc(10,s,st)
  if (st /= 0) call abort
  call fputc(10,"2",st)
  if (st /= 0) call abort
  call fputc(10,"3 ",st)
  if (st /= 0) call abort
  rewind(10)
  call fgetc(10,s)
  if (s(1:1) /= "1") call abort
  call fgetc(10,s)
  if (s(1:1) /= "2") call abort
  call fgetc(10,s,st)
  if ((s(1:1) /= "3") .or. (st /= 0)) call abort
  call fgetc(10,s,st)
  if (st /= -1) call abort
  close (10)

! FGETC and FPUTC on units not opened should not work
  call fgetc(12,s,st)
  if (st /= -1) call abort
  call fputc(12,s,st)
  if (st /= -1) call abort
  end
