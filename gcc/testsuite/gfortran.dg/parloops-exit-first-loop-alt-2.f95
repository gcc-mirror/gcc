! { dg-additional-options "-O2" }
! { dg-require-effective-target pthread }
! { dg-additional-options "-ftree-parallelize-loops=2" }
! { dg-additional-options "-fdump-tree-parloops" }

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

! Three times plus 25:
! - once in f._loopfn.0
! - once in the parallel
! - once in the low iteration count loop
! Crucially, none for a peeled off last iteration following the parallel.
! { dg-final { scan-tree-dump-times "(?n) \\+ 25;" 3 "parloops" } }
