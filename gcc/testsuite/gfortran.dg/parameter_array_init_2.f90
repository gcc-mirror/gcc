! { dg-do run }
! { dg-options "-std=gnu" } ! suppress the warning about line 15
! Thrashes the fix for PR29400, where the scalar initializers
! were not expanded to arrays with the appropriate shape.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
  integer,parameter :: i(1,1) = 0, j(2) = 42

  if (any (maxloc(j+j,mask=(j==2)) .ne. 0)) call abort ()
  if (size(j+j) .ne. 2) call abort ()
  if (minval(j+j) .ne. 84) call abort ()
  if (minval(j,mask=(j==2)) .ne. huge (j)) call abort ()
  if (maxval(j+j) .ne. 84) call abort ()
  if (maxval(j,mask=(j==2)) .ne. -huge (j)-1) call abort ()
  if (sum(j,mask=j==2) .ne. 0) call abort ()
  if (sum(j+j) .ne. 168) call abort ()
  if (product(j+j) .ne. 7056) call abort ()
  if (any(ubound(j+j) .ne. 2)) call abort ()
  if (any(lbound(j+j) .ne. 1)) call abort ()
  if (dot_product(j+j,j) .ne. 7056) call abort ()
  if (dot_product(j,j+j) .ne. 7056) call abort ()
  if (count(i==1) .ne. 0) call abort ()
  if (any(i==1)) call abort ()
  if (all(i==1)) call abort ()
  end
