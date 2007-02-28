! { dg-do compile }
! PR 30869 - pointer loop variables were wrongly rejected.
program main
  integer, pointer :: i
  allocate (i)
  do i=1,10
  end do
  deallocate (i)
end program main
