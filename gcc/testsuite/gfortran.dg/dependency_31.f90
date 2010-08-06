! { dg-do compile }
! { dg-options "-Warray-temporaries" }
! PR 45159 - make sure no temporary is created for this.
subroutine foo(a,n,i,j)
  implicit none
  integer, intent(in) :: i,j,n
  real, dimension(20) :: a
  a(1:10) = a(i:j)
  a(20:n:-3) = a(n:i:-3)
end subroutine foo
