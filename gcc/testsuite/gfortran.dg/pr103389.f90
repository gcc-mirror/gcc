! { dg-do run }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer, allocatable :: a(:)
   end type
   type(t) :: y
   y%a = [1,2]
   call s((y))
   if (any (y%a .ne. [3,4])) stop 1
contains
   subroutine s(x)
      class(*) :: x
      select type (x)
        type is (t)
          x%a = x%a + 2
        class default
          stop 2
      end select
   end
end
