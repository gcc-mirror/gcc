! { dg-do compile }
!
! Test the fix for PR104555 in which the select type statement caused an
! ICE because the selector expression was type(t) rather than class(t).
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      character(:), allocatable :: a
   end type
   call s(t("abcd"))
   call s([t("efgh")])
contains
   subroutine s(x)
      class(t) :: x(..)
      select rank (x)
      rank (0)
         print *, "|", x%a, "|"
         select type (y => x)
         type is (t)
           print *, "|", y%a, "|"
         end select
      rank (1)
         print *, "|", x(1)%a, "|"
         select type (y => x)
         type is (t)
           print *, "|", y(1)%a, "|"
         end select
      end select
   end
end
