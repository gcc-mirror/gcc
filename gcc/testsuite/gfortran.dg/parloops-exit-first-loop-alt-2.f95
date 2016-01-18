! { dg-additional-options "-O2" }
! { dg-require-effective-target pthread }
! { dg-additional-options "-ftree-parallelize-loops=2" }
! { dg-additional-options "-fdump-tree-parloops2-details" }

! Constant bound, vector addition.

subroutine foo ()
  integer, parameter :: n = 1000
  integer, dimension (0:n-1) :: a, b, c
  common a, b, c
  integer :: ii

  do ii = 0, n - 1
     c(ii) = a(ii) + b(ii) + 25
  end do
end subroutine foo

! { dg-final { scan-tree-dump-times "alternative exit-first loop transform succeeded" 1 "parloops2" } }
