! { dg-do run }
! Tests the fix for PR33686, in which dependencies were not
! correctly handled for the assignments below.
!
! Contributed by Dick Hendrickson on comp.lang.fortran,
! " Most elegant syntax for inverting a permutation?" 20071006
!
! Test the fix for PR36091 as well...
! { dg-options "-fbounds-check" }
!
  integer :: p(4) = (/2,4,1,3/)
  forall (i = 1:4) p(p(i)) = i                ! This was the original
  if (any (p .ne. (/3,1,4,2/))) call abort ()

  forall (i = 1:4) p(5 - p(i)) = p(5 - i)     ! This is a more complicated version
  if (any (p .ne. (/1,2,3,4/))) call abort ()
end
