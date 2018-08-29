! { dg-do run }
! PR 48955 - missing array temporary when there was both a forward
! and a backward dependency.
! Test case slightly modified from the original one by Kacper Kowalik.
program ala
   implicit none

   integer, parameter  :: n = 6
   real, dimension(n), parameter :: result = [1.,10.,30.,90.,270., 243.];
   real, dimension(n) :: v0, v1
   character(len=80) :: line1, line2

   v0 = [1.0, 3.0, 9.0, 27.0, 81.0, 243.0]
   v1 = v0

   v1(2:n-1) = v1(1:n-2) + v1(3:n)
   if (any(v1 /= result)) STOP 1
   v1 = v0
   v1(2:n-1) = v0(1:n-2) + v0(3:n)
   if (any(v1 /= result)) STOP 2

   v1 = v0
   v1(2:n-1) = v1(3:n) + v1(1:n-2)
   if (any(v1 /= result)) STOP 3
   v1 = v0
   v1(2:n-1) = v0(3:n) + v0(1:n-2)
   if (any(v1 /= result)) STOP 4

end program ala
