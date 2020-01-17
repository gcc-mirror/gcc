! { dg-do run }
!
! PR fortran/84135
!
! Co-contributed by G. Steinmetz
!
! Ensure that coarray shape remains correct
! after merging the shape from 'dimension'
!
program p
   integer :: i
   integer, dimension(3) :: x[2,*]
   data (x(i:i+2:i+1), i=1,2) /1,2,3/
   integer, dimension(3) :: y[2,3,-3:4,5,7:*] = [1,2,3]
   integer :: z, z2[2:4,7:9,-2:2,-7:8,-4:*]
   codimension :: z[2:4,7:9,-2:2,-7:8,-4:*]
   integer, codimension[1:*] :: z3[2:4,7:9,-2:2,-7:8,-4:*]
   dimension :: z(1:2,-3:-2,7:7), z2(1:2,-3:-2,7:7), z3(1:2,-3:-2,7:7)
   integer, codimension[2:4,7:9,-2:2,-7:8,-4:*], dimension(1:2,-3:-2,7:7) :: z4
   integer, codimension[*], dimension(1:2,-3:-2,7:7) :: z5[2:4,7:9,-2:2,-7:8,-4:*]
   integer, codimension[2:4,7:9,-2:2,-7:8,-4:*], dimension(3) :: z6(1:2,-3:-2,7:7)
   integer, codimension[*], dimension(4) :: z7(1:2,-3:-2,7:7)[2:4,7:9,-2:2,-7:8,-4:*]

   if (any (lcobound(x) /= [1, 1])) stop 1
   if (any (lcobound(y) /= [1, 1, -3, 1, 7])) stop 3
   if (any (lcobound(z) /= [2,7,-2,-7,-4])) stop 4
   if (any (lcobound(z2) /= lcobound(z))) stop 4
   if (any (lcobound(z3) /= lcobound(z))) stop 5
   if (any (lcobound(z4) /= lcobound(z))) stop 6
   if (any (lcobound(z5) /= lcobound(z))) stop 7
   if (any (lcobound(z6) /= lcobound(z))) stop 8
   if (any (lcobound(z7) /= lcobound(z))) stop 9

   if (any (lbound(x) /= [1])) stop 11
   if (any (lbound(y) /= [1])) stop 12
   if (any (lbound(z) /= [1,-3,7])) stop 13
   if (any (lbound(z2) /= lbound(z))) stop 14
   if (any (lbound(z3) /= lbound(z))) stop 15
   if (any (lbound(z4) /= lbound(z))) stop 16
   if (any (lbound(z5) /= lbound(z))) stop 17
   if (any (lbound(z6) /= lbound(z))) stop 18
   if (any (lbound(z7) /= lbound(z))) stop 19

   if (any (ubound(x) /= [3])) stop 21
   if (any (ubound(y) /= [3])) stop 22
   if (any (ubound(z) /= [2,-2,7])) stop 23
   if (any (ubound(z2) /= ubound(z))) stop 24
   if (any (ubound(z3) /= ubound(z))) stop 25
   if (any (ubound(z4) /= ubound(z))) stop 26
   if (any (ubound(z5) /= ubound(z))) stop 27
   if (any (ubound(z6) /= ubound(z))) stop 28
   if (any (ubound(z7) /= ubound(z))) stop 29

   if (any (ucobound(z2) /= ucobound(z))) stop 31
   if (any (ucobound(z3) /= ucobound(z))) stop 32
   if (any (ucobound(z4) /= ucobound(z))) stop 33
   if (any (ucobound(z5) /= ucobound(z))) stop 34
   if (any (ucobound(z6) /= ucobound(z))) stop 35
   if (any (ucobound(z7) /= ucobound(z))) stop 36

   if (num_images() == 1) then
     if (any (ucobound(x) /= [2, lbound(x,dim=1)])) stop 37
     if (any (ucobound(y) /= [2, 3, 4, 5, 7])) stop 38
     if (any (ucobound(z) /= [4,9,2,8,-4])) stop 39
   else
     if (ucobound(x, dim=1) /= 2) stop 41
     if (ucobound(y, dim=1) /= 2) stop 42
     if (ucobound(y, dim=2) /= 3) stop 43
     if (ucobound(y, dim=3) /= 4) stop 44
     if (ucobound(y, dim=4) /= 5) stop 45
     if (ucobound(z, dim=1) /= 4) stop 46
     if (ucobound(z, dim=2) /= 9) stop 47
     if (ucobound(z, dim=3) /= 2) stop 48
     if (ucobound(z, dim=4) /= 8) stop 49
   endif
end
