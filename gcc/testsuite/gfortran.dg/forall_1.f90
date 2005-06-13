! { dg-do run }
! tests FORALL statements with a mask
dimension i2(15,10), i1(15)
type a
   sequence
   integer k
end type a
type(a) :: a1(10), a2(5,5)

i1 = (/ 0, 1, 2, 3, 4, 0, 6, 7, 8, 9, 10, 0, 0, 13, 14 /)
forall (i=1:15, i1(i) /= 0)
   i1(i) = 0
end forall
if (any(i1 /= 0)) call abort

a1(:)%k = i1(1:10)
forall (i=1:10, a1(i)%k == 0)
   a1(i)%k = i
end forall
if (any (a1(:)%k /= (/ (i, i=1,10) /))) call abort

forall (i=1:15, j=1:10, a1(j)%k <= j)
   i2(i,j) = j + i*11
end forall
do i=1,15
   if (any (i2(i,:) /= (/ (i*11 + j, j=1,10) /))) call abort
end do
end
