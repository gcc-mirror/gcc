! { dg-do run }
! PR 15957
! we used to return the wrong shape when the order parameter was used
! in reshape.
!
INTEGER, parameter :: i(2,3) = reshape ((/1,2,3,4,5,6/), (/2,3/)), &
     j(2,4) = reshape ((/1,2,3,4,5,6/), (/2,4/), (/0,0/), (/2,1/))

integer :: k(2,3), m(2,4), n(2,3), o(2,4)

k(1,:) = (/ 1, 3, 5 /)
k(2,:) = (/ 2, 4, 6 /)

m(1,:) = (/ 1, 2, 3, 4 /)
m(2,:) = (/ 5, 6, 0, 0 /)

! check that reshape does the right thing while constant folding
if (any(i /= k)) STOP 1
if (any(j /= m)) STOP 2

! check that reshape does the right thing at runtime
n = reshape ((/1,2,3,4,5,6/), (/2,3/))
if (any(n /= k)) STOP 3
o = reshape ((/1,2,3,4,5,6/), (/2,4/), (/0,0/), (/2,1/))
if (any(o /= m)) STOP 4
end 

