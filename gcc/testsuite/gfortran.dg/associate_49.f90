! { dg-do run }
!
! Test the fix for PR91588, in which the declaration of 'a' caused
! an ICE.
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
program p
   character(4), parameter :: parm = '7890'
   associate (z => '1234')
      block
         integer(len(z)) :: a
         if (kind(a) .ne. 4) stop 1
      end block
   end associate
   associate (z => '123')
      block
         integer(len(z)+1) :: a
         if (kind(a) .ne. 4) stop 2
      end block
   end associate
   associate (z => 1_8)
      block
         integer(kind(z)) :: a
         if (kind(a) .ne. 8) stop 3
      end block
   end associate
   associate (z => parm)
      block
         integer(len(z)) :: a
         if (kind(a) .ne. 4) stop 4
      end block
   end associate
end
