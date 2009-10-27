! { dg-do compile }
! { dg-require-effective-target vect_float }

! Peeling to align the store to Y will also align the load from Y.
! The load from X may still be misaligned.

SUBROUTINE SAXPY(X, Y, A)
DIMENSION X(64), Y(64)
Y = Y + A * X
END

! { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } 
! { dg-final { scan-tree-dump-times "Alignment of access forced using peeling" 1 "vect" { xfail { { vect_no_align } || {! vector_alignment_reachable} } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 1 "vect" { xfail { { vect_no_align } || {! vector_alignment_reachable} } } } }
! { dg-final { scan-tree-dump-times "Vectorizing an unaligned access" 2 "vect" { target { {! vector_alignment_reachable} && {! vect_hw_misalign} } } } }
! { dg-final { scan-tree-dump-times "accesses have the same alignment." 1 "vect" } }
! { dg-final { cleanup-tree-dump "vect" } }
