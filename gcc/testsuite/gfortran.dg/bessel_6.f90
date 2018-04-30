! { dg-do run { xfail spu-*-* } }
! { dg-add-options ieee }
!
! PR fortran/36158
! PR fortran/33197
!
! XFAILed for SPU targets since we don't have an accurate library
! implementation of the single-precision Bessel functions.
!
! Run-time tests for transformations BESSEL_JN
!
implicit none
real,parameter :: values(*) = [0.0, 0.5, 1.0, 0.9, 1.8,2.0,3.0,4.0,4.25,8.0,34.53, 475.78] 
real,parameter :: myeps(size(values)) = epsilon(0.0) &
                  * [2, 7, 5, 6, 9, 12, 12, 7, 7, 8, 98, 15 ]
! The following is sufficient for me - the values above are a bit
! more tolerant
!                  * [0, 5, 3, 4, 6, 7, 7, 5, 5, 6, 66, 4 ]
integer,parameter :: mymax(size(values)) =  &
                 [100, 17, 23, 21, 27, 28, 32, 35, 31, 41, 47, 37 ]
integer, parameter :: Nmax = 100
real :: rec(0:Nmax), lib(0:Nmax)
integer :: i

do i = 1, ubound(values,dim=1)
  call compare(mymax(i), values(i), myeps(i))
end do

contains

subroutine compare(mymax, X, myeps)

integer :: i, nit, mymax
real X, myeps, myeps2

rec(0:mymax) = BESSEL_JN(0, mymax, X)
lib(0:mymax) = [ (BESSEL_JN(i, X), i=0,mymax) ]

!print *, 'YN for X = ', X, ' -- Epsilon = ',epsilon(x)
do i = 0, mymax
!  print '(i2,2e17.9,e12.2,f18.10,2l3)', i, rec(i), lib(i), &
!        rec(i)-lib(i),           ((rec(i)-lib(i))/rec(i))/epsilon(x), &
!        rec(i) == lib(i), abs((rec(i)-lib(i))/rec(i)) < myeps
if (rec(i) == lib(i)) CYCLE
if (abs((rec(i)-lib(i))/rec(i)) > myeps) &
  STOP 1
end do

end
end
