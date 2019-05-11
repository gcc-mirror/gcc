! { dg-do run }
! { dg-options "-O3 -fdump-tree-original" }
! Tests the fix for PR33850, in which one of the two assignments
! below would produce an unnecessary temporary for the index
! expression, following the fix for PR33749.
!
! Contributed by Dick Hendrickson on comp.lang.fortran,
! " Most elegant syntax for inverting a permutation?" 20071006
!
  integer(4) :: p4(4) = (/2,4,1,3/)
  integer(4) :: q4(4) = (/2,4,1,3/)
  integer(8) :: p8(4) = (/2,4,1,3/)
  integer(8) :: q8(4) = (/2,4,1,3/)
  p4(q4) = (/(i, i = 1, 4)/)
  q4(q4) = (/(i, i = 1, 4)/)
  p8(q8) = (/(i, i = 1, 4)/)
  q8(q8) = (/(i, i = 1, 4)/)
  if (any(p4 .ne. q4)) STOP 1
  if (any(p8 .ne. q8)) STOP 2
end
! Whichever is the default length for array indices will yield
! parm 18 times, because a temporary is not necessary.  The other
! cases will all yield a temporary, so that atmp appears 18 times.
! Note that it is the kind conversion that generates the temp.
!
! { dg-final { scan-tree-dump-times "parm" 20 "original" } }
! { dg-final { scan-tree-dump-times "atmp" 20 "original" } }
