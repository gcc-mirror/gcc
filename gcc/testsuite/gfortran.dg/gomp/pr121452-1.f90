! { dg-do compile }

! Check that the front end acccepts a CONTINUE statement
! inside an ordered loop.

implicit none
integer :: i, j
integer :: A(5,5), B(5,5) = 1

!$omp do ordered(2)
   do 10 i = 1, 5
     do 20 j = 1, 5
       A(i,j) = B(i,j)
20   continue
10 continue

if (any(A /= 1)) stop 1
end
