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
   if (res /= 'FTT') call abort
   write (unit=res,fmt=f) any(m2,dim=1)
   if (res /= 'FTT') call abort
   write (unit=res,fmt=f) any(m4,dim=1)
   if (res /= 'FTT') call abort
   write (unit=res,fmt=f) any(m8,dim=1)
   if (res /= 'FTT') call abort
   write (unit=res,fmt=f) any(m1,dim=2)
   if (res /= 'TTT') call abort
   write (unit=res,fmt=f) any(m2,dim=2)
   if (res /= 'TTT') call abort
   write (unit=res,fmt=f) any(m4,dim=2)
   if (res /= 'TTT') call abort
   write (unit=res,fmt=f) any(m8,dim=2)
   if (res /= 'TTT') call abort

   write (unit=res,fmt=f) all(m1,dim=1)
   if (res /= 'FFT') call abort
   write (unit=res,fmt=f) all(m2,dim=1)
   if (res /= 'FFT') call abort
   write (unit=res,fmt=f) all(m4,dim=1)
   if (res /= 'FFT') call abort
   write (unit=res,fmt=f) all(m8,dim=1)
   if (res /= 'FFT') call abort

   write (unit=res,fmt=f) all(m1,dim=2)
   if (res /= 'FFF') call abort
   write (unit=res,fmt=f) all(m2,dim=2)
   if (res /= 'FFF') call abort
   write (unit=res,fmt=f) all(m4,dim=2)
   if (res /= 'FFF') call abort
   write (unit=res,fmt=f) all(m8,dim=2)
   if (res /= 'FFF') call abort

   write (unit=res,fmt=g) count(m1,dim=1)
   if (res /= '023') call abort
   write (unit=res,fmt=g) count(m2,dim=1)
   if (res /= '023') call abort
   write (unit=res,fmt=g) count(m4,dim=1)
   if (res /= '023') call abort
   write (unit=res,fmt=g) count(m8,dim=1)
   if (res /= '023') call abort

   write (unit=res,fmt=g) count(m1,dim=2)
   if (res /= '221') call abort
   write (unit=res,fmt=g) count(m2,dim=2)
   if (res /= '221') call abort
   write (unit=res,fmt=g) count(m4,dim=2)
   if (res /= '221') call abort
   write (unit=res,fmt=g) count(m8,dim=2)
   if (res /= '221') call abort

end program main
