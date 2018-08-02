/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fdump-tree-vect-details -mtune=skylake" } */

#include "avx2-gather-1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 16 "vect" } } */
