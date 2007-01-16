! PR fortran/30404
! Checks that we correctly handle nested masks in nested FORALL blocks.
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
! { dg-do run }
  logical :: l1(2,2)
  integer :: it(2,2)
  l1(:,:) = reshape ((/.false.,.true.,.true.,.false./), (/2,2/))
  it(:,:) = reshape ((/1,2,3,4/), (/2,2/))
  forall (i = 1:2, i < 3)
    forall (j = 1:2, l1(i,j))
      it(i, j) = 0
    end forall
  end forall
!  print *, l1
!  print '(4i2)', it
  if (any (it .ne. reshape ((/1, 0, 0, 4/), (/2, 2/)))) call abort ()
end
