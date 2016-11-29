! { dg-do run "xfail *-*-*" }
! { dg-options "" }
!
! Make sure we still see an error for missing exponents without -fdec.
!

implicit none

real :: r
character(2) :: s
s = '8e'

read (s, *) r ! { XFAIL "Bad real number" }

end
