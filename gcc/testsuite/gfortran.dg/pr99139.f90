! { dg-do compile }
! { dg-options "-finit-local-zero" }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
! Original implicitly typed 'x' gave a bad symbol ICE
subroutine s1(x)
   target :: x(..)
   select rank (y => x)
   rank (1)
   rank (2)
   end select
end

! Comment #2: Failed with above option
subroutine s2(x, z)
   real, target :: x(..)
   real :: z(10)
   select rank (y => x) ! Error was:Assumed-rank variable y at (1) may only be
                        ! used as actual argument
   rank (1)
   rank (2)
   end select
end
