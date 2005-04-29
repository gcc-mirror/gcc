module entry_8_m
type t
  integer i
  real x (5)
end type t
end module entry_8_m

function f (i)
  use entry_8_m
  type (t) :: f,g
  f % i = i
  return
  entry g (x)
  g%x = x
end function f

use entry_8_m
type (t) :: f, g, res

res = f (42)
if (res%i /= 42) call abort ()
res = g (1.)
if (any (res%x /= 1.)) call abort ()
end
