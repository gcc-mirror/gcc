! { dg-do compile }
! PR 15975, PR 16606
! Pointers to derived types with initialized components
!
! Contributed by Erik Edelmann <erik.edelmann@iki.fi>
!
SUBROUTINE N
  TYPE T
    INTEGER :: I = 99
  END TYPE T
  TYPE(T), POINTER :: P
  TYPE(T), TARGET  :: Q
  P => Q
  if (P%I.ne.99) call abort ()
END SUBROUTINE N

program test_pr15975
  call n ()
end program test_pr15975

