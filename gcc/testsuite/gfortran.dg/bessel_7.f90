! { dg-do run { xfail *-*-mingw* spu-*-* } }
! { dg-add-options ieee }
!
! PR fortran/36158
! PR fortran/33197
!
! For mingw targets this test is disabled as the MS implementation
! of BESSEL_YN(n,x) has different results.  It returns NAN rather than
! -INF for "x=0.0" and all "n".
!
! XFAILed for SPU targets since we don't have an accurate library
! implementation of the single-precision Bessel functions.
!
! Run-time tests for transformations BESSEL_YN
!
implicit none
real,parameter :: values(*) = [0.0, 0.5, 1.0, 0.9, 1.8,2.0,3.0,4.0,4.25,8.0,34.53, 475.78] 
real,parameter :: myeps(size(values)) = epsilon(0.0) &
                  * [2, 3, 4, 5, 8, 2, 13, 6, 7, 6, 36, 168 ]
! The following is sufficient for me - the values above are a bit
! more tolerant
!                  * [0, 0, 0, 3, 3, 0, 9, 0, 2, 1, 22, 130 ]
integer,parameter :: nit(size(values)) =  &
                 [100, 100, 100, 25, 15, 100, 10, 31, 7, 100, 7, 25 ]
integer, parameter :: Nmax = 100
real :: rec(0:Nmax), lib(0:Nmax)
integer :: i

do i = 1, ubound(values,dim=1)
  call compare(values(i), myeps(i), nit(i), 6*epsilon(0.0))
end do

contains

subroutine compare(X, myeps, nit, myeps2)

integer :: i, nit
real X, myeps, myeps2

rec = BESSEL_YN(0, Nmax, X)
lib = [ (BESSEL_YN(i, X), i=0,Nmax) ]

!print *, 'YN for X = ', X, ' -- Epsilon = ',epsilon(x)
do i = 0, Nmax
!  print '(i2,2e17.9,e12.2,f14.10,2l3)', i, rec(i), lib(i), &
!        rec(i)-lib(i), ((rec(i)-lib(i))/rec(i))/epsilon(x), &
!        i > nit .or. rec(i) == lib(i) &
!                .or. abs((rec(i)-lib(i))/rec(i)) < myeps2, &
!        rec(i) == lib(i) .or. abs((rec(i)-lib(i))/rec(i)) < myeps
if (.not. (i > nit .or. rec(i) == lib(i) &
                   .or. abs((rec(i)-lib(i))/rec(i)) < myeps2)) &
  call abort ()
if (.not. (rec(i) == lib(i) .or. abs((rec(i)-lib(i))/rec(i)) < myeps)) &
  call abort ()
end do

end
end
