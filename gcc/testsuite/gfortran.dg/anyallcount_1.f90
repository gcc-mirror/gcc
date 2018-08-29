! { dg-do run }
program main
   character(len=*), parameter :: f='(3L1)'
   character(len=*), parameter :: g='(3I1)'
   real, dimension(3,3) :: a
   logical(kind=1), dimension(3,3) :: m1
   logical(kind=2), dimension(3,3) :: m2
   logical(kind=4), dimension(3,3) :: m4
   logical(kind=8), dimension(3,3) :: m8
   character(len=3) :: res
   data a /-1.0, -2.0, -3.0, 2.0, 1.0, -2.1, 1.0, 2.0, 3.0 /

   m1 = a > 0
   m2 = a > 0
   m4 = a > 0
   m8 = a > 0

   write (unit=res,fmt=f) any(m1,dim=1)
   if (res /= 'FTT') STOP 1
   write (unit=res,fmt=f) any(m2,dim=1)
   if (res /= 'FTT') STOP 2
   write (unit=res,fmt=f) any(m4,dim=1)
   if (res /= 'FTT') STOP 3
   write (unit=res,fmt=f) any(m8,dim=1)
   if (res /= 'FTT') STOP 4
   write (unit=res,fmt=f) any(m1,dim=2)
   if (res /= 'TTT') STOP 5
   write (unit=res,fmt=f) any(m2,dim=2)
   if (res /= 'TTT') STOP 6
   write (unit=res,fmt=f) any(m4,dim=2)
   if (res /= 'TTT') STOP 7
   write (unit=res,fmt=f) any(m8,dim=2)
   if (res /= 'TTT') STOP 8

   write (unit=res,fmt=f) all(m1,dim=1)
   if (res /= 'FFT') STOP 9
   write (unit=res,fmt=f) all(m2,dim=1)
   if (res /= 'FFT') STOP 10
   write (unit=res,fmt=f) all(m4,dim=1)
   if (res /= 'FFT') STOP 11
   write (unit=res,fmt=f) all(m8,dim=1)
   if (res /= 'FFT') STOP 12

   write (unit=res,fmt=f) all(m1,dim=2)
   if (res /= 'FFF') STOP 13
   write (unit=res,fmt=f) all(m2,dim=2)
   if (res /= 'FFF') STOP 14
   write (unit=res,fmt=f) all(m4,dim=2)
   if (res /= 'FFF') STOP 15
   write (unit=res,fmt=f) all(m8,dim=2)
   if (res /= 'FFF') STOP 16

   write (unit=res,fmt=g) count(m1,dim=1)
   if (res /= '023') STOP 17
   write (unit=res,fmt=g) count(m2,dim=1)
   if (res /= '023') STOP 18
   write (unit=res,fmt=g) count(m4,dim=1)
   if (res /= '023') STOP 19
   write (unit=res,fmt=g) count(m8,dim=1)
   if (res /= '023') STOP 20

   write (unit=res,fmt=g) count(m1,dim=2)
   if (res /= '221') STOP 21
   write (unit=res,fmt=g) count(m2,dim=2)
   if (res /= '221') STOP 22
   write (unit=res,fmt=g) count(m4,dim=2)
   if (res /= '221') STOP 23
   write (unit=res,fmt=g) count(m8,dim=2)
   if (res /= '221') STOP 24

end program main
