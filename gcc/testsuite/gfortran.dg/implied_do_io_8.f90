! { dg-do run }
! { dg-additional-options "-fcheck=bounds" }
! PR fortran/111837 - out of bounds access with front-end optimization

program implied_do_bug
  implicit none
  integer :: i,j,k
  real    :: arr(1,1,1)
  integer :: ni(1)
  ni(1) = 1
  arr = 1
  write(*,*) (((arr(i,j,k), i=1,ni(k)), k=1,1), j=1,1)
  write(*,*) (((arr(i,j,k), i=1,ni(k)), j=1,1), k=1,1)
  write(*,*) (((arr(k,i,j), i=1,ni(k)), k=1,1), j=1,1)
  write(*,*) (((arr(k,i,j), i=1,ni(k)), j=1,1), k=1,1)
  write(*,*) (((arr(j,k,i), i=1,ni(k)), k=1,1), j=1,1)
  write(*,*) (((arr(j,k,i), i=1,ni(k)), j=1,1), k=1,1)
end
