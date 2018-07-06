! { dg-do run }

  implicit none
  integer i
  real rda1(10), rda(10), rval
  double precision dda1(10), dda(10), dval

  rda = (/ 1,2,3,4,5,6,7,8,9,10 /)
  rDA1 = MOD (1.1*(rDA(1)-5.0), P=(rDA-2.5))
  DO i = 1, 10
    rVAL = MOD (1.1*(rDA(1)-5.0), P=(rDA(i)-2.5))
    if (rval /= rda1(i)) STOP 1
  enddo

  dda = (/ 1,2,3,4,5,6,7,8,9,10 /)
  dDA1 = MOD (1.1d0*(dDA(1)-5.0d0), P=(dDA-2.5d0))
  DO i = 1, 10
    dVAL = MOD (1.1d0*(dDA(1)-5.0d0), P=(dDA(i)-2.5d0))
    if (dval /= dda1(i)) STOP 2
  enddo

end
