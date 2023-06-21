! { dg-do compile }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   associate (a => 1)
      print *, [character((a(1))) :: '1'] ! { dg-error "has an array reference" }
   end associate
end
