! { dg-do run }
! { dg-options "-std=gnu" } ! suppress the warning about line 15
! Thrashes the fix for PR29400, where the scalar initializers
! were not expanded to arrays with the appropriate shape.
!
! Contributed by Francois-Xavier Coudert <fxcoudert@gcc.gnu.org>
!
  integer,parameter :: i(1,1) = 0, j(2) = 42

  if (any (maxloc(j+j,mask=(j==2)) .ne. 0)) STOP 1
  if (size(j+j) .ne. 2) STOP 2
  if (minval(j+j) .ne. 84) STOP 3
  if (minval(j,mask=(j==2)) .ne. huge (j)) STOP 4
  if (maxval(j+j) .ne. 84) STOP 5
  if (maxval(j,mask=(j==2)) .ne. -huge (j)-1) STOP 6
  if (sum(j,mask=j==2) .ne. 0) STOP 7
  if (sum(j+j) .ne. 168) STOP 8
  if (product(j+j) .ne. 7056) STOP 9
  if (any(ubound(j+j) .ne. 2)) STOP 10
  if (any(lbound(j+j) .ne. 1)) STOP 11
  if (dot_product(j+j,j) .ne. 7056) STOP 12
  if (dot_product(j,j+j) .ne. 7056) STOP 13
  if (count(i==1) .ne. 0) STOP 14
  if (any(i==1)) STOP 15
  if (all(i==1)) STOP 16
  end
