! { dg-do run }
! PR 20074: This used to segfault at runtime.
! Test case contributed by "Alfredo Buttari" <pitagoras@tin.it>

program tryreshape

  integer,allocatable :: vect1(:),resh1(:,:)
  integer,pointer :: vect(:),resh(:,:)
  integer :: vect2(2*4), resh2(2,4)
  integer :: r, s(2)

  r=2;  nb=4

  s(:)=(/r,nb/)

  allocate(vect(nb*r),vect1(nb*r))
  allocate(resh(r,nb),resh1(r,nb))

  vect =1
  vect1=1
  vect2=1

  resh2 = reshape(vect2,s)
  if (resh2(1,1) /= 1.0) call abort

  resh1 = reshape(vect1,s)
  if (resh1(1,1) /= 1.0) call abort

  resh = reshape(vect,s)
  if (resh(1,1) /= 1.0) call abort

end program tryreshape
