! { dg-do  run }
! { dg-additional-options "-fdump-tree-original -ffrontend-optimize" }
! PR 65819 - this used to cause a temporary in matmul inlining.
! Check that these are absent by looking for the names of the
! temporary variables.
program main
  implicit none
  real, dimension(3,3,3) :: f
  real, dimension(3,3) :: res
  real, dimension(2,3,3) :: backup
  integer :: three
  integer :: i

  data f(1,:,:) /9*-42./
  data f(2:3,:,:) /2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61/
  data res /652, 772, 984, 2010, 2406, 3082, 3402, 4086, 5242/ 
  three = 3
  backup = f(2:3,:,:)
  f(1, 1:three, :) = matmul(f(2,1:3,2:3), f(3,2:3,:))
  if (any (res /= f(1,:,:))) stop 1
  if (any (f(2:3,:,:) /= backup)) stop 2
end program main
! { dg-final { scan-tree-dump-not "mma" "original" } }
! { dg-final { scan-tree-dump-not "mmb" "original" } }
