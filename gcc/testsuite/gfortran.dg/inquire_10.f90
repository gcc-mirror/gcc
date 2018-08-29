! { dg-do run { target { ! newlib } } }
  character(len=800) :: cwd
  integer :: unit

  call getcwd(cwd)

  open(file='cseq', unit=23)
  inquire(file='cseq',number=unit)
  if (unit /= 23) STOP 1
  inquire(file=trim(cwd) // '/cseq',number=unit)
  if (unit /= 23) STOP 2

  close(unit=23, status = 'delete')

  inquire(file='foo/../cseq2',number=unit)
  if (unit >= 0) STOP 3
  inquire(file='cseq2',number=unit)
  if (unit >= 0) STOP 4
end
