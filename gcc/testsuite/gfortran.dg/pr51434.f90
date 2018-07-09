! { dg-do run }
! PR fortran/51434
module foo
   implicit none
   integer, parameter :: n = 5
   character(len=1), parameter :: s(n) = 'a'
   type :: a
      integer :: m = n
      character(len=1):: t(n) = transfer('abcde', s)
   end type a
end module foo

program bar
   use foo
   implicit none
   type(a) c
   if (c%m /= n) stop 1
   if (any(c%t /= ['a', 'b', 'c', 'd', 'e'])) stop 2
end program bar
