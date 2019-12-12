! { dg-do  run }
! { dg-options "-ffrontend-optimize" }
! PR 86837 - this was mis-optimized by trying to turn this into an
! array I/O statement.
! Original test case by "Pascal".

Program read_loop

  implicit none

  integer :: i, j

  ! number of values per column
  integer, dimension(3) :: nvalues
  data nvalues / 1, 2, 4 /

  ! values in a 1D array
  real, dimension(7) :: one_d
  data one_d / 1,   11, 12,   21, 22, 23, 24 /

  ! where to store the data back
  real, dimension(4, 3) :: two_d

  ! 1 - write our 7 values in one block
  open(unit=10, file="loop.dta", form="unformatted")
  write(10) one_d
  close(unit=10)

  ! 2 - read them back in chosen cells of a 2D array
  two_d = -9
  open(unit=10, file="loop.dta", form="unformatted", status='old')
  read(10) ((two_d(i,j), i=1,nvalues(j)), j=1,3)
  close(unit=10, status='delete')

  ! 4 - print the whole array, just in case

  if (any(reshape(two_d,[12]) /= [1.,-9.,-9.,-9.,11.,12.,-9.,-9.,21.,22.,23.,24.])) call abort

end Program read_loop
