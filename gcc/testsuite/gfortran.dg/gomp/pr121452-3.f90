! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

! Check that the OMP_STRUCTURED_BLOCK that wraps intervening code is accepted by
! the OMP lowering pass.


implicit none
integer :: i, j
integer :: A(5,5), B(5,5) = 1

!$omp simd collapse(2)
   do 10 i = 1, 5
     do 20 j = 1, 5
       A(i,j) = B(i,j)
20   continue
10 continue

if (any(A /= 1)) stop 1
end

! { dg-final { scan-tree-dump "#pragma omp __structured_block" "original" } }
