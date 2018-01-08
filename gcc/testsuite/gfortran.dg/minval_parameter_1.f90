! { dg-do  run }
! Test for run-time simplification of minval
program main
  implicit none
  integer, dimension(2,3), parameter :: i = &
       & reshape([-1,2,-3,5,-7,11], shape(i))
  integer, dimension(3), parameter :: im1 = minval(i,dim=1)
  integer, parameter :: im2 = minval(i,mask=i>4)
  integer, dimension(2), parameter :: im3 = minval(i,dim=2)
  integer, parameter :: im4 = minval(i, mask=i>-1)
  integer, dimension(3), parameter :: im5 = minval(i,dim=1,mask=i>4)
  integer, dimension(2), parameter :: im6 = minval(i,dim=2,mask=i>4)

  real, dimension(2,3), parameter :: r = &
       & reshape([-1.,2.,-3.,5.,-7.,11.], shape(r))
  real, dimension(3), parameter :: rm1 = minval(r,dim=1)
  real, parameter :: rm2 = minval(r,mask=r>4)
  real, dimension(2), parameter :: rm3 = minval(r,dim=2)
  real, parameter :: rm4 = minval(r, mask=r>-1)
  real, dimension(3), parameter :: rm5 = minval(r,dim=1,mask=r>4)
  real, dimension(2), parameter :: rm6 = minval(r,dim=2,mask=r>4)

  character(len=3), parameter :: maxv = achar(255) // achar(255) // achar(255)
  character(len=3), dimension(2,3), parameter :: c = &
       reshape(["asd", "fgh", "qwe", "jkl", "ert", "zui"], shape(c))
  character(len=3), parameter :: cm1 = minval(c)
  character(len=3), dimension(3), parameter :: cm2 = minval(c,dim=1)
  character(len=3), dimension(2), parameter :: cm3 = minval(c,dim=2)
  character(len=3), parameter :: cm4 = minval (c, c>"g")
  character(len=3), dimension(3), parameter :: cm5 = minval(c,dim=1,mask=c>"g")

  if (any (im1 /= [ -1, -3, -7])) call abort
  if (im2 /= 5) call abort
  if (any (im3 /= [ -7,2])) call abort
  if (im4 /= 2) call abort
  if (any (im5 /= [huge(im5), 5, 11])) call abort
  if (any (im6 /= [huge(im6), 5])) call abort

  if (any (rm1 /= [ -1., -3., -7.])) call abort
  if (rm2 /= 5) call abort
  if (any (rm3 /= [ -7.,2.])) call abort
  if (rm4 /= 2) call abort
  if (any (rm5 /= [huge(rm5), 5., 11.])) call abort
  if (any (rm6 /= [huge(rm6), 5.])) call abort

  if (cm1 /= "asd") call abort
  if (any (cm2 /= ["asd", "jkl", "ert" ])) call abort
  if (any (cm3 /= ["asd", "fgh" ])) call abort
  if (cm4 /= "jkl") call abort
  if (any(cm5 /= [ maxv, "jkl", "zui" ] )) call abort
end program main
