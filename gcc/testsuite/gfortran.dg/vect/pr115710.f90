! { dg-do compile }
! { dg-additional-options "-Ofast" }
! { dg-require-effective-target vect_float }
! { dg-require-effective-target vect_call_sqrtf }

! { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } }
! CABS expansion should allow for the vectorization to happen.

subroutine foo(a,b,n)
  complex(kind(1.0))::a(*)
  real(kind(1.0))::b(*)
  integer::i,n

  do i=1,n
     b(i)=abs(a(i))**2
  end do

end subroutine foo
