! { dg-additional-options "--param openacc-kernels=decompose" }

! A regression test checking that the reduction clause lowering does
! not fail if a subroutine argument is used as a reduction variable in
! a kernels region.

! This was fine ...
subroutine reduction_var_not_argument(res)
  real res
  real tmp
  integer i

  !$acc kernels
  !$acc loop reduction(+:tmp)
  do i=0,n-1
     tmp = tmp + 1
  end do
  !$acc end kernels

  res = tmp
end subroutine reduction_var_not_argument

! ... but this led to problems because ARG
! was a pointer type that did not get dereferenced.
subroutine reduction_var_as_argument(arg)
  real arg
  integer i

  !$acc kernels
  !$acc loop reduction(+:arg)
  do i=0,n-1
     arg = arg + 1
  end do
  !$acc end kernels
end subroutine reduction_var_as_argument


