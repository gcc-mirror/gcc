! { dg-do run }
! Test to make sure that PR 33354 remains fixed and doesn't regress
PROGRAM TST
  IMPLICIT NONE
  REAL :: A(1,3)
  REAL :: B(3,1)
  A(:,1) = 10
  A(:,2) = 20
  A(:,3) = 30

  !WRITE(*,*) SUM(A(:,1:3),1)
  !WRITE(*,*) MINLOC(SUM(A(:,1:3),1),1)
  if (minloc(sum(a(:,1:3),1),1) .ne. 1) call abort()
  if (maxloc(sum(a(:,1:3),1),1) .ne. 3) call abort()

  B(1,:) = 10
  B(2,:) = 20
  B(3,:) = 30
  if (minloc(sum(b(1:3,:),2),2) .ne. 1) call abort()
  if (maxloc(sum(b(1:3,:),2),2) .ne. 3) call abort()
END PROGRAM TST
