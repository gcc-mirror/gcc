! { dg-do run }
!
! PR fortran/92872
!
! Contributed by G. Steinmetz
!
module m
contains
subroutine s(x) bind(c)
   integer, allocatable, optional :: x(:)
   x = [1, 2, 3]
end
end

use m
integer, allocatable :: y(:)
! NOTE: starting at 0, otherwise it will fail due to PR 92189
allocate(y(0:2))
y = [9, 8, 7]
call s(y)
if (any (y /= [1, 2, 3])) stop 1
end
