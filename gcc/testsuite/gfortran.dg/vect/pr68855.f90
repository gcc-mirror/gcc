! { dg-do compile }
! { dg-require-effective-target vect_float }

! { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } }
! PAREN_EXPR should not cause the vectorization of complex float add to be missed.

subroutine foo(a,n)

  complex (kind(1.0)) :: a(*)
  integer :: i,n

  do i=1,n
     a(i)=(a(i)+(6.0,1.0))
  enddo
  
end subroutine foo
