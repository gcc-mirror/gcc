! { dg-do run }
program main
  character(len=3), dimension(2) :: a
  a(1) = 'aaa'
  a(2) = 'bbb'
  if (maxloc(a,dim=1) /= 2) call abort
  if (minloc(a,dim=1) /= 1) call abort

end program main
