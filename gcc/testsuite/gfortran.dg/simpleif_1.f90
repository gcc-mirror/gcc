! { dg-do run }
! PR 17074
! Verifies that FORALL and WHERE after a simple if work.
DIMENSION ia(4,4)
logical,dimension(4,4) :: index

if (.true.) forall (i = 1:4, j = 1:4) ia(i,j) = 1
if (any (ia.ne.1)) CALL abort()

index(:,:)=.false.
index(2,3) = .true.

if (.true.) where (index) ia = 2
if (ia(2,3).ne.2) call abort()

end
