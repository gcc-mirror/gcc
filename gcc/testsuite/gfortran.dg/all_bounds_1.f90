! { dg-do run }
! { dg-options "-fbounds-check" }
! { dg-shouldfail "Incorrect extent in return value of ALL intrinsic" }
program main
  logical(kind=4), allocatable :: f(:,:)
  logical(kind=4) :: res(3)
  character(len=80) line
  allocate (f(2,2))
  f = .false.
  f(1,1) = .true.
  f(2,1) = .true.
  res = all(f,dim=1)
  write(line,fmt='(80L1)') res
end program main
! { dg-output "Fortran runtime error: Incorrect extent in return value of ALL intrinsic in dimension 1: is 3, should be 2" }


