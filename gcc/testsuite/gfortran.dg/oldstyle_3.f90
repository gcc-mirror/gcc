! { dg-do compile }
! Suppress the warning about an old-style initializer;
! { dg-options "" }
! This tests the fix for PR29052 in which the error below would cause a seg-fault
! because the locus of the initializer was never set.
!
! Contributed by Bud Davis  <bdavis@gcc.gnu.org>
!
       character*10 a(4,2) /'aaa','bbb','ccc','ddd'/ ! { dg-error "more variables than values" }
       end
