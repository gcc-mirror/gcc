! { dg-do compile }
! { dg-require-effective-target vect_float }

SUBROUTINE SAXPY(X, Y, A, N)
DIMENSION X(N), Y(N)
Y = Y + A * X
END

! fail to vectorize due to failure to compute number of iterations (PR tree-optimization/18527)
! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail *-*-* } } } 
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail *-*-* } } } 
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { xfail *-*-* } } } 
! { dg-final { cleanup-tree-dump "vect" } }
