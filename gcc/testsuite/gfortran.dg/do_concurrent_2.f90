! { dg-do run }
!
! PR fortran/44646
!
! DO CONCURRENT
!
implicit none
integer :: i, j
integer :: A(5,5)

A = 0.0
do concurrent (i=1:5, j=1:5, (i/=j))
  if (i == 5) cycle
  A(i,j) = i*j
end do

if (any (A(:,1) /= [0,  2,  3,  4, 0])) STOP 1
if (any (A(:,2) /= [2,  0,  6,  8, 0])) STOP 2
if (any (A(:,3) /= [3,  6,  0, 12, 0])) STOP 3
if (any (A(:,4) /= [4,  8, 12,  0, 0])) STOP 4
if (any (A(:,5) /= [5, 10, 15, 20, 0])) STOP 5

A = -99

do concurrent (i = 1 : 5)
  forall (j=1:4, i/=j)
    A(i,j) = i*j
  end forall
  if (i == 5) then
    A(i,i) = -i
  end if
end do

if (any (A(:,1) /= [-99,   2,   3,   4,  5])) STOP 6
if (any (A(:,2) /= [  2, -99,   6,   8, 10])) STOP 7
if (any (A(:,3) /= [  3,   6, -99,  12, 15])) STOP 8
if (any (A(:,4) /= [  4,   8,  12, -99, 20])) STOP 9
if (any (A(:,5) /= [-99, -99, -99, -99, -5])) STOP 10

end
