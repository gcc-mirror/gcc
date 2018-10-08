! { dg-do run }
!
! Test the fix for PR86372 in which the associate name string length was
! not being set, thereby causing a segfault.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
program xxx

   character(len=50) :: s

   s = repeat ('*', len(s))
   call sub(s)
   if (s .ne. '**'//'123'//repeat ('*', len(s) - 5)) stop 1

contains

   subroutine sub(str)
      character(len=*), intent(inout) :: str
      associate (substr => str(3:5))
         substr = '123'
      end associate
   end subroutine

end
