! { dg-do compile }
! { dg-options "-O0" }
! PR20902 - Overlapping initializers in an equivalence block must
! have the same value.
!
! The code was replaced completely after the fix for PR30875, which
! is a repeat of the original and comes from the same contributor.
! The fix for 20902 was wrong.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  TYPE T1
    sequence
    integer :: i=1
  END TYPE T1
  TYPE T2           ! OK because initializers are equal
    sequence
    integer :: i=1
  END TYPE T2
  TYPE T3
    sequence
    integer :: i=2
  END TYPE T3
  TYPE(T1) :: a1
  TYPE(T2) :: a2
  TYPE(T3) :: a3
  EQUIVALENCE (a1, a2)
  EQUIVALENCE (a1, a3) ! { dg-error "Overlapping unequal initializers" }
  write(6, *) a1, a2, a3
END

