/* { dg-do compile } */
/* { dg-options "-msse4.1 -O3 -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 6 "vect"} } */
#include "sse2-pr39821.c"
