! { dg-do run }
! { dg-options "-funsigned" }
! Test dshiftl, dshiftr, ibclr, ibset and ibits intrinsics.
program main
  unsigned :: u, v, w
  integer :: i, j, k

  u = 1u;  v = 4u
  i = 1;   j = 4
  if (int(dshiftl(u,v,12)) /= dshiftl(i,j,12)) error stop 1
  if (int(dshiftl(1u,4u,12)) /= dshiftl(1,4,12)) error stop 2
  if (int(dshiftr(u,v,12)) /= dshiftr(i,j,12)) error stop 3
  if (int(dshiftr(1u,4u,12)) /= dshiftr(1,4,12)) error stop 4

  k = 14

  if (int(dshiftl(u,v,k)) /= dshiftl(i,j,k)) error stop 5
  if (int(dshiftl(1u,4u,k)) /= dshiftl(1,4,k)) error stop 6
  if (int(dshiftr(u,v,k)) /= dshiftr(i,j,k)) error stop 7
  if (int(dshiftr(1u,4u,k)) /= dshiftr(1,4,k)) error stop 8

  u = 255u
  i = 255
  do k=0,8
     if (ibclr(i,k) /= int(ibclr(u,k))) error stop  9
     if (ibset(i,k+4) /= int(ibset(u,k+4))) error stop 10
  end do
  if (ibclr(255,5) /= int(ibclr(255u,5))) error stop 11
  if (ibset(255,10) /= int(ibset(255u,10))) error stop 12

  if (uint(ibits(6,6,2)) /= ibits(6u,6,2)) error stop 13
end program main
