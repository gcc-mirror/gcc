! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of MAXLOC intrinsic in dimension 1: is 3, should be 2" }
program main
  integer(kind=4), allocatable :: f(:,:)
  logical, allocatable :: m(:,:)
  integer(kind=4) :: res(3)
  character(len=80) line
  allocate (f(2,2),m(2,2))
  f = 3
  m = .true.
  res = maxloc(f,dim=1,mask=m)
  write(line,fmt='(80I1)') res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of MAXLOC intrinsic in dimension 1: is 3, should be 2" }

