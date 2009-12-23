! { dg-do run }
! PR 42422 - read with a repeat specifyer following a separator
program main
  integer, dimension(10) :: i1, i2

  i1 = 0
  i2 = (/ 1, 2, 3, 5, 5, 5, 5, 0, 0, 0 /)
  open (10,file="pr42422.dat")
  write (10,'(A)') ' 1 2 3 4*5 /'
  rewind 10
  read (10,*) i1
  if (any(i1 /= i2)) call abort
  close (10,status="delete")
end program main
