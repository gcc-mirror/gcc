! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! PR 92569 - the EOF condition was not recognized with
! -ffrontend-optimize.  Originjal test case by Bill Lipa.
program main
  implicit none
  real(kind=8) ::  tdat(1000,10)
  real(kind=8) :: res (10, 3)
  integer :: i, j, k, np

  open (unit=20, status="scratch")
  res = reshape([(real(i),i=1,30)], shape(res))
  write (20,'(10G12.5)') res
  rewind 20
  do  j = 1,1000
     read (20,*,end=1)(tdat(j,k),k=1,10)
  end do
      
1 continue
  np = j-1
  if (np /= 3) stop 1
  if (any(transpose(res) /= tdat(1:np,:))) stop 2
end program main
