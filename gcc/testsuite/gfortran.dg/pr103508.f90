! { dg-do compile }
!
! Check the fix for PR103508. As noted in comment 6 of the PR, the bug
! has nothing to do with PDTs. However, the contributor's test has been
! retained.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer :: n = 1
      character(3) :: c
   end type
   block
      block
         type(t) :: x
         x%c = 'abc'
         print *, len(x%c)
      end ! { dg-error "END BLOCK statement expected" }
   end    ! { dg-error "END BLOCK statement expected" }
end
! { dg-error "Unexpected end of file" "" { target "*-*-*" } 0 }
