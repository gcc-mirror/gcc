/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fdump-tree-vect-details" } */

#include "avx2-gather-1.c"

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loops in function" 16 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
