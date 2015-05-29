! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/38184
! invariant RESHAPE not expanded if SOURCE is empty.
!
! Original program by James Van Buskirk

integer, parameter :: N = 3
integer, parameter :: A(N,N) = reshape([integer::],[N,N],reshape([1],[N+1],[2]))
integer, parameter :: K = N*A(2,2)+A(2,3)
integer :: B(N,N) = reshape([1,2,2,2,1,2,2,2,1],[3,3])
integer :: i
i = 5
if (any(A /= B)) call abort
if (K /= i) call abort
end

! { dg-final { scan-tree-dump-times "\\\{1, 2, 2, 2, 1, 2, 2, 2, 1\\\}" 2 "original" } }
