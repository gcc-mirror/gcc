! PR 13077: we used to fail when reading the module
module m1
real, dimension(4) :: a
data a(1:3:2) /2*1.0/
end module m1
use m1
if (a(1).NE.1.) STOP 1
if (a(1).NE.a(3)) STOP 2
end
