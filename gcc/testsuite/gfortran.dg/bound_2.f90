! { dg-do run }
! PR fortran/29391
! This file is here to check that LBOUND and UBOUND return correct values
!
! Contributed by Francois-Xavier Coudert (coudert@clipper.ens.fr)
  implicit none
  integer :: i(-1:1,-1:1) = 0
  integer :: j(-1:2) = 0

  if (any(lbound(i(-1:1,-1:1)) /= 1)) call abort
  if (any(ubound(i(-1:1,-1:1)) /= 3)) call abort
  if (any(lbound(i(:,:)) /= 1)) call abort
  if (any(ubound(i(:,:)) /= 3)) call abort
  if (any(lbound(i(0:,-1:)) /= 1)) call abort
  if (any(ubound(i(0:,-1:)) /= [2,3])) call abort
  if (any(lbound(i(:0,:0)) /= 1)) call abort
  if (any(ubound(i(:0,:0)) /= 2)) call abort

  if (any(lbound(transpose(i)) /= 1)) call abort
  if (any(ubound(transpose(i)) /= 3)) call abort
  if (any(lbound(reshape(i,[2,2])) /= 1)) call abort
  if (any(ubound(reshape(i,[2,2])) /= 2)) call abort
  if (any(lbound(cshift(i,-1)) /= 1)) call abort
  if (any(ubound(cshift(i,-1)) /= 3)) call abort
  if (any(lbound(eoshift(i,-1)) /= 1)) call abort
  if (any(ubound(eoshift(i,-1)) /= 3)) call abort
  if (any(lbound(spread(i,1,2)) /= 1)) call abort
  if (any(ubound(spread(i,1,2)) /= [2,3,3])) call abort
  if (any(lbound(maxloc(i)) /= 1)) call abort
  if (any(ubound(maxloc(i)) /= 2)) call abort
  if (any(lbound(minloc(i)) /= 1)) call abort
  if (any(ubound(minloc(i)) /= 2)) call abort
  if (any(lbound(maxval(i,2)) /= 1)) call abort
  if (any(ubound(maxval(i,2)) /= 3)) call abort
  if (any(lbound(minval(i,2)) /= 1)) call abort
  if (any(ubound(minval(i,2)) /= 3)) call abort
  if (any(lbound(any(i==1,2)) /= 1)) call abort
  if (any(ubound(any(i==1,2)) /= 3)) call abort
  if (any(lbound(count(i==1,2)) /= 1)) call abort
  if (any(ubound(count(i==1,2)) /= 3)) call abort
  if (any(lbound(merge(i,i,.true.)) /= 1)) call abort
  if (any(ubound(merge(i,i,.true.)) /= 3)) call abort
  if (any(lbound(lbound(i)) /= 1)) call abort
  if (any(ubound(lbound(i)) /= 2)) call abort
  if (any(lbound(ubound(i)) /= 1)) call abort
  if (any(ubound(ubound(i)) /= 2)) call abort
  if (any(lbound(shape(i)) /= 1)) call abort
  if (any(ubound(shape(i)) /= 2)) call abort

  if (any(lbound(product(i,2)) /= 1)) call abort
  if (any(ubound(product(i,2)) /= 3)) call abort
  if (any(lbound(sum(i,2)) /= 1)) call abort
  if (any(ubound(sum(i,2)) /= 3)) call abort
  if (any(lbound(matmul(i,i)) /= 1)) call abort
  if (any(ubound(matmul(i,i)) /= 3)) call abort
  if (any(lbound(pack(i,.true.)) /= 1)) call abort
  if (any(ubound(pack(i,.true.)) /= 9)) call abort
  if (any(lbound(unpack(j,[.true.],[2])) /= 1)) call abort
  if (any(ubound(unpack(j,[.true.],[2])) /= 1)) call abort

  call sub1(i,3)
  call sub1(reshape([7,9,4,6,7,9],[3,2]),3)

contains

  subroutine sub1(a,n)
    integer :: a(2:n+1,4:*), n
    if (any([lbound(a,1), lbound(a,2)] /= [2, 4])) call abort
    if (any(lbound(a) /= [2, 4])) call abort
  end subroutine sub1

end
