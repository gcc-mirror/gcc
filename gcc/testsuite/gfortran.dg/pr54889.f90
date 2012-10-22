! PR tree-optimization/54889
! { dg-do compile }
! { dg-options "-O3" }
! { dg-additional-options "-mavx" { target { i?86-*-* x86_64-*-* } } }

subroutine foo(x,y,z)
  logical, pointer :: x(:,:)
  integer :: y, z
  x=x(1:y,1:z)
end subroutine
