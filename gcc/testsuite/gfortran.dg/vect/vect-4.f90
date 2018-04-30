! { dg-do compile }
! { dg-require-effective-target vect_float }
! { dg-additional-options "--param vect-max-peeling-for-alignment=0" }

! Peeling to align the store to Y will also align the load from Y.
! The load from X may still be misaligned.

SUBROUTINE SAXPY(X, Y, A)
DIMENSION X(64), Y(64)
Y = Y + A * X
END

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } 
! { dg-final { scan-tree-dump-times "accesses have the same alignment." 1 "vect" } }
