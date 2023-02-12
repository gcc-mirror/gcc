! { dg-do compile }
! { dg-options "-O2 -fdump-tree-dom2" }

subroutine bar(n,m,p,s)
implicit none
integer :: n,m
real,intent(inout) :: p(n),s(*)
call foo(n,m,p,s)
call foo(n,m,p,s)
end subroutine bar

subroutine foo(n,m,p,b)
implicit none
integer :: n,m,j
real,intent(inout) :: p(n),b(*)
!GCC$ ATTRIBUTES noinline :: foo
do j=1,n
  b(m+j-1)=p(j)
enddo
m=m+n
end subroutine foo

! { dg-final { scan-tree-dump-times "foo \\(" 4 "dom2"} }
