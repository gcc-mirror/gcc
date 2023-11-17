! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in MASK argument of MAXLOC intrinsic in dimension 2: is 3, should be 2|Array bound mismatch for dimension 2 of array 'f' .2/3." }
program main
  integer(kind=4), allocatable :: f(:,:)
  logical, allocatable :: m(:,:)
  integer(kind=4) :: res(2)
  character(len=80) line
  allocate (f(2,2),m(2,3))
  f = 3
  m = .true.
  res = maxloc(f,dim=1,mask=m)
  write(line,fmt='(80I1)') res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in MASK argument of MAXLOC intrinsic in dimension 2: is 3, should be 2|Array bound mismatch for dimension 2 of array 'f' .2/3." }

