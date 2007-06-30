! { dg-do compile }
!
! PR32472 -- character literals were not implemented in REPEAT.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
  CHARACTER(len=1025) :: string2 = repeat('?',1025)
  print *, string2
end
