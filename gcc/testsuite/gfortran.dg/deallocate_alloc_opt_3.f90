! { dg-do run }
program a

  implicit none

  integer n
  character(len=70) e1
  character(len=30) e2
  integer, allocatable :: i(:)

  e1 = 'No error'
  allocate(i(4))
  deallocate(i, stat=n, errmsg=e1)
  if (trim(e1) /= 'No error') call abort

  e2 = 'No error'
  allocate(i(4))
  deallocate(i, stat=n, errmsg=e2)
  if (trim(e2) /= 'No error') call abort

  e1 = 'No error'
  deallocate(i, stat=n, errmsg=e1)
  if (trim(e1) /= 'Attempt to deallocate an unallocated object') call abort

  e2 = 'No error'
  deallocate(i, stat=n, errmsg=e2)
  if (trim(e2) /= 'Attempt to deallocate an unall') call abort

end program a
