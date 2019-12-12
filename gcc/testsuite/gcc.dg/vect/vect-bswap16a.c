/* { dg-additional-options "-msse2 -mno-sse3" { target sse2_runtime } } */

#include "vect-bswap16.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { vect_shift } } } } */
