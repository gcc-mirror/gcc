! { dg-do run }
!
! PR fortran/43228
!
integer :: a(3,3)
character(len=100) :: str
namelist /nml/a

a = -1
str = '&nml a(1,:) = 1 2 3 /'
read(str, nml=nml)
if (any (a(1,:) /= [1, 2, 3])) STOP 1
if (any (a([2,3],:) /= -1)) STOP 2

a = -1
str = '&nml a(1,1) = 1 2 3 4 /'
read(str, nml=nml)
if (any (a(:,1) /= [1, 2, 3])) STOP 3
if (any (a(:,2) /= [4, -1, -1])) STOP 4
if (any (a(:,3) /= -1)) STOP 5

str = '&nml a(1,:) = 1 2 3 , &
       &    a(2,:) = 4,5,6 &
       &    a(3,:) = 7 8 9/'
read(str, nml=nml)
if (any (a(1,:) /= [1, 2, 3])) STOP 6
if (any (a(2,:) /= [4, 5, 6])) STOP 7
if (any (a(3,:) /= [7, 8, 9])) STOP 8

!print *, a(:,1)
!print *, a(:,2)
!print *, a(:,3)
end


