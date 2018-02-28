!  Check in_pack and in_unpack for integer and comlex types, with
!  alignment issues thrown in for good measure.

program main
  implicit none

  complex(kind=4) :: a4(5),b4(5),aa4(5),bb4(5)
  real(kind=4) :: r4(100)
  equivalence(a4(1),r4(1)),(b4(1),r4(12))

  complex(kind=8) :: a8(5),b8(5),aa8(5),bb8(5)
  real(kind=8) :: r8(100)
  equivalence(a8(1),r8(1)),(b8(1),r8(12))

  integer(kind=4) :: i4(5),ii4(5)
  integer(kind=8) :: i8(5),ii8(5)

  integer :: i

  a4 = (/(cmplx(i,-i,kind=4),i=1,5)/)
  b4 = (/(2*cmplx(i,-i,kind=4),i=1,5)/)
  call csub4(a4(5:1:-1),b4(5:1:-1),5)
  aa4 = (/(cmplx(5-i+1,i-5-1,kind=4),i=1,5)/)
  if (any(aa4 /= a4)) STOP 1
  bb4 = (/(2*cmplx(5-i+1,i-5-1,kind=4),i=1,5)/)
  if (any(bb4 /= b4)) STOP 2

  a8 = (/(cmplx(i,-i,kind=8),i=1,5)/)
  b8 = (/(2*cmplx(i,-i,kind=8),i=1,5)/)
  call csub8(a8(5:1:-1),b8(5:1:-1),5)
  aa8 = (/(cmplx(5-i+1,i-5-1,kind=8),i=1,5)/)
  if (any(aa8 /= a8)) STOP 3
  bb8 = (/(2*cmplx(5-i+1,i-5-1,kind=8),i=1,5)/)
  if (any(bb8 /= b8)) STOP 4

  i4 = (/(i, i=1,5)/)
  call isub4(i4(5:1:-1),5)
  ii4 = (/(5-i+1,i=1,5)/)
  if (any(ii4 /= i4)) STOP 5

  i8 = (/(i,i=1,5)/)
  call isub8(i8(5:1:-1),5)
  ii8 = (/(5-i+1,i=1,5)/)
  if (any(ii8 /= i8)) STOP 6

end program main

subroutine csub4(a,b,n)
  implicit none
  complex(kind=4), dimension(n) :: a,b
  complex(kind=4), dimension(n) :: aa, bb
  integer :: n, i
  aa = (/(cmplx(n-i+1,i-n-1,kind=4),i=1,n)/)
  if (any(aa /= a)) STOP 7
  bb = (/(2*cmplx(n-i+1,i-n-1,kind=4),i=1,5)/)
  if (any(bb /= b)) STOP 8
  a = (/(cmplx(i,-i,kind=4),i=1,5)/)
  b = (/(2*cmplx(i,-i,kind=4),i=1,5)/)
end subroutine csub4

subroutine csub8(a,b,n)
  implicit none
  complex(kind=8), dimension(n) :: a,b
  complex(kind=8), dimension(n) :: aa, bb
  integer :: n, i
  aa = (/(cmplx(n-i+1,i-n-1,kind=8),i=1,n)/)
  if (any(aa /= a)) STOP 9
  bb = (/(2*cmplx(n-i+1,i-n-1,kind=8),i=1,5)/)
  if (any(bb /= b)) STOP 10
  a = (/(cmplx(i,-i,kind=8),i=1,5)/)
  b = (/(2*cmplx(i,-i,kind=8),i=1,5)/)
end subroutine csub8

subroutine isub4(a,n)
  implicit none
  integer(kind=4), dimension(n) :: a
  integer(kind=4), dimension(n) :: aa
  integer :: n, i
  aa = (/(n-i+1,i=1,n)/)
  if (any(aa /= a)) STOP 11
  a = (/(i,i=1,5)/)
end subroutine isub4

subroutine isub8(a,n)
  implicit none
  integer(kind=8), dimension(n) :: a
  integer(kind=8), dimension(n) :: aa
  integer :: n, i
  aa = (/(n-i+1,i=1,n)/)
  if (any(aa /= a)) STOP 12
  a = (/(i,i=1,5)/)
end subroutine isub8
