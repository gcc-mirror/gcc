! { dg-do run }
!
! Test the fix for PR83898.f90
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
program p
   associate (x => ['1','2'])
      if (any (x .ne. ['1','2'])) STOP 1
   end associate
end
