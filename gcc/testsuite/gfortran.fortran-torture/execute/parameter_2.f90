module m
  parameter (p = -1.) ! negative numbers used to get output incorrectly
end module m

use m
if (p .ne. -1.) CALL abort()
end
