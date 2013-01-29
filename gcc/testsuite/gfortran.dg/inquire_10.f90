! { dg-do run { target { ! newlib } } }
  character(len=800) :: cwd
  integer :: unit

  call getcwd(cwd)

  open(file='cseq', unit=23)
  inquire(file='cseq',number=unit)
  if (unit /= 23) call abort
  inquire(file=trim(cwd) // '/cseq',number=unit)
  if (unit /= 23) call abort

  inquire(file='foo/../cseq2',number=unit)
  if (unit >= 0) call abort
  inquire(file='cseq2',number=unit)
  if (unit >= 0) call abort
end
