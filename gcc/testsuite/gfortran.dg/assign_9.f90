! { dg-do run }
! Tests the fix for PR33749, in which one of the two assignments
! below would not produce a temporary for the index expression.
!
! Contributed by Dick Hendrickson on comp.lang.fortran,
! " Most elegant syntax for inverting a permutation?" 20071006
!
  integer(4) :: p(4) = (/2,4,1,3/)
  integer(8) :: q(4) = (/2,4,1,3/)
  p(p) = (/(i, i = 1, 4)/)
  q(q) = (/(i, i = 1, 4)/)
  if (any(p .ne. q)) call abort ()
end

