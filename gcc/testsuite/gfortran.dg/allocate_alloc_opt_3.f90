! { dg-do run }
program a

  implicit none

  integer n
  character(len=70) e1
  character(len=30) e2
  integer, allocatable :: i(:)

  e1 = 'No error'
  allocate(i(4), stat=n, errmsg=e1)
  if (trim(e1) /= 'No error') call abort
  deallocate(i)

  e2 = 'No error'
  allocate(i(4),stat=n, errmsg=e2)
  if (trim(e2) /= 'No error') call abort
  deallocate(i)


  e1 = 'No error'
  allocate(i(4), stat=n, errmsg=e1)
  allocate(i(4), stat=n, errmsg=e1)
  if (trim(e1) /= 'Attempt to allocate an allocated object') call abort
  deallocate(i)

  e2 = 'No error'
  allocate(i(4), stat=n, errmsg=e2)
  allocate(i(4), stat=n, errmsg=e2)
  if (trim(e2) /= 'Attempt to allocate an allocat') call abort

end program a
