/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fno-common -fdump-tree-vect-details  -mtune=haswell -fno-split-loops" } */

#include "avx2-gather-5.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 1 "vect" } } */
