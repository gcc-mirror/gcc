! { dg-do  run }
! Test for run-time simplification of maxval
program main
  implicit none
  integer, dimension(2,3), parameter :: i = &
       & reshape([-1,2,-3,5,-7,11], shape(i))
  integer, dimension(3), parameter :: im1 = maxval(i,dim=1)
  integer, parameter :: im2 = maxval(i,mask=i<0)
  integer, dimension(2), parameter :: im3 = maxval(i,dim=2)
  integer, parameter :: im4 = maxval(i, mask=i<-1)
  integer, dimension(3), parameter :: im5 = maxval(i,dim=1,mask=i<-2)
  integer, dimension(2), parameter :: im6 = maxval(i,dim=2,mask=i<0)

  real, dimension(2,3), parameter :: r = &
       & reshape([-1.,2.,-3.,5.,-7.,11.], shape(r))
  real, dimension(3), parameter :: rm1 = maxval(r,dim=1)
  real, parameter :: rm2 = maxval(r,mask=r<0)
  real, dimension(2), parameter :: rm3 = maxval(r,dim=2)
  real, parameter :: rm4 = maxval(r, mask=r<-1)
  real, dimension(3), parameter :: rm5 = maxval(r,dim=1,mask=r<-2)
  real, dimension(2), parameter :: rm6 = maxval(r,dim=2,mask=r<0)

  character(len=3), parameter :: minv = achar(0) // achar(0) // achar(0)
  character(len=3), dimension(2,3), parameter :: c = &
       reshape(["asd", "fgh", "qwe", "jkl", "ert", "zui"], shape(c))
  character(len=3), parameter :: cm1 = maxval(c)
  character(len=3), dimension(3), parameter :: cm2 = maxval(c,dim=1)
  character(len=3), dimension(2), parameter :: cm3 = maxval(c,dim=2)
  character(len=3), parameter :: cm4 = maxval (c, c<"g")
  character(len=3), dimension(3), parameter :: cm5 = maxval(c,dim=1,mask=c<"p")

  if (any (im1 /= [ 2, 5, 11])) STOP 1
  if (im2 /= -1) STOP 2
  if (any (im3 /= [ -1,11])) STOP 3
  if (im4 /= -3) STOP 4
  if (any (im5 /= [-huge(im5)-1, -3, -7])) STOP 5! { dg-warning "Integer outside symmetric range" }
  if (any (im6 /= [-1, -huge(im6)-1])) STOP 6! { dg-warning "Integer outside symmetric range" }

  if (any (rm1 /= [ 2., 5., 11.])) STOP 7
  if (rm2 /= -1.) STOP 8
  if (any (rm3 /= [ -1.,11.])) STOP 9
  if (rm4 /= -3.) STOP 10
  if (any (rm5 /= [-huge(rm5), -3., -7.])) STOP 11
  if (any (rm6 /= [-1.,-huge(rm6)])) STOP 12

  if (cm1 /= "zui") STOP 13
  if (any (cm2 /= ["fgh", "qwe", "zui" ])) STOP 14
  if (any (cm3 /= ["qwe", "zui" ])) STOP 15
  if (cm4 /= "fgh") STOP 16
  if (any(cm5 /= [ "fgh", "jkl", "ert" ] )) STOP 17
end program main
