! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR126909, in which END PROGRAM/FUNCTION/SUBROUTINE labels
! were not being generated correctly, when preceded by contain sections.
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
Module tally
  integer :: ctr = 0
contains
  integer function two_power_n_plus (n, base)
    integer, intent(IN) :: n, base
    two_power_n_plus = 2**n  + base
  end function two_power_n_plus
end module

program lost
   use tally
   interface
      function yet_more_lost() result(i)
         integer :: i
      end function yet_more_lost
   end interface

   goto 9
   stop 1
9  ctr = two_power_n_plus (1, ctr)
   call sub()
   call more_lost()
   if (yet_more_lost () /= 42) stop 2
   if (ctr /= 126) stop 3
   goto 10
   stop 4
   contains
      subroutine sub()
         ctr = two_power_n_plus (2, ctr)
         goto 11
         stop 5
11    end subroutine sub
10 end program lost

subroutine more_lost
   use tally
   goto 9
   stop 6
9  ctr = two_power_n_plus (3, ctr)
   call sub2()
   goto 10
   stop 7
   contains
      subroutine sub2()
         ctr = two_power_n_plus (4, ctr)
         goto 11
         stop 8
11    end subroutine sub2
10 end subroutine more_lost

function yet_more_lost() result(i)
   use tally
   integer :: i
   i = 42
   goto 9
   stop 9
9  ctr = two_power_n_plus (5, ctr)
   call sub3()
   goto 10
   stop 10
   contains
      subroutine sub3()
         ctr = two_power_n_plus (6, ctr)
         goto 11
         stop 11
11    end subroutine sub3
10 end function yet_more_lost

! { dg-final { scan-tree-dump-times "label.000010" 6 "original" } }
