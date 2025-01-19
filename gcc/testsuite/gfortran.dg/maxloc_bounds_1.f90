! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of MAXLOC intrinsic in dimension 1: is 3, should be 2|Array bound mismatch for dimension 1 of array 'res' .3/2." }
program main
  integer(kind=4), allocatable :: f(:,:)
  integer(kind=4) :: res(3)
  character(len=80) line
  allocate (f(2,2))
  f = 3
  res = maxloc(f,dim=1)
  write(line,fmt='(80I1)') res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of MAXLOC intrinsic in dimension 1: is 3, should be 2|Array bound mismatch for dimension 1 of array 'res' .3/2." }

