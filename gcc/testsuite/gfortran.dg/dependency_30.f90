! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 45159 - make sure no temporary is created for this.
subroutine foo(a,b,i,j,k,n)
  implicit none
  integer, intent(in) :: i, j, k, n
  real, dimension(n) :: a,b
  a(k:i-1) = a(i:j)
end subroutine foo
