! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,i,j,k)
  integer, dimension (10) :: a
  integer :: i, j, k

  a(1:5:2) = a(8:6:-1)

  a(1:8) = a(2:9)

  a(4:7) = a(4:1:-1)

  a(i:i+2) = a(i+4:i+6)

  a(j:1:-1) = a(j:5)

  a(k:k+2) = a(k+1:k+3)
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
