! { dg-do run }
!
! Test the fix for PR95541, in which parameterized array and string components
! of PDT arrays caused an ICE in the ASSOCIATE selector expressions below.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t(n)
      integer, len :: n
      integer :: a(n)
      character(len = n) :: chr
   end type
   type(t(3)) :: x(2)
   integer :: tgt(2)
   x(1)%a = [1, 2, 3]
   x(1)%chr = "abc"
   x(2)%a = [4, 5, 6]
   x(2)%chr = "def"
   associate (y => x(:)%a(3))
      if (any (y /= [3,6]))          stop 1
      y = -y
   end associate
   associate (y => x%a(3))
      if (any (y /= [-3,-6]))        stop 2
      y = -y * 10
   end associate
   if (any (x%a(3) /= [30,60]))      stop 3
   if (any (x%a(2) /= [2,5]))        stop 4
   associate (y => x%chr(2:2))
      if (any (y /= ["b","e"]))      stop 5
      y = ["x", "y"]
   end associate
   if (any (x%chr /= ["axc","dyf"])) stop 6
end
