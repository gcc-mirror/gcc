! { dg-do compile }
! { dg-options "-O2 -fno-tree-pre -fpredictive-commoning -fdump-tree-pcom-details" }
subroutine trisolve2(x,i1,i2,nxyz)
integer :: nxyz
real,dimension(nxyz):: au1
real,allocatable,dimension(:) :: gi
integer :: i1 , i2
real,dimension(i2)::x
integer :: i
allocate(gi(nxyz))
do i = i1+1 , i2
   x(i) = gi(i)*(x(i)-au1(i-1)*x(i-1))
enddo
end subroutine trisolve2
! { dg-final { scan-tree-dump "Executing predictive commoning" "pcom" } }
